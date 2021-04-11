package fpinscala.localeffects

object fp14 {

    sealed trait ST[S, A] {
        self =>
        protected def run(s: S): (A, S)

        def map[B](f: A => B): ST[S, B] = new ST[S, B] {
            def run(s: S) = {
                val (a, s1) = self.run(s)
                (f(a), s1)
            }
        }

        def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
            def run(s: S) = {
                val (a, s1) = self.run(s)
                f(a).run(s1)
            }
        }
    }

    object ST {
        def runST[A](st: RunnableST[A]): A =
            st.apply[Unit].run(())._1

        def apply[S, A](a: => A) = {
            lazy val memo = a
            new ST[S, A] {
                def run(s: S) = (memo, s)
            }
        }
    }

    sealed trait STRef[S, A] {
        protected var cell: A

        def read: ST[S, A] = ST(cell)

        def write(a: A): ST[S, Unit] = new ST[S, Unit] {
            def run(s: S) = {
                cell = a
                ((), s)
            }
        }
    }

    object STRef {
        def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
            var cell = a
        })
    }

    sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
        protected def value: Array[A]

        def size: ST[S, Int] = ST(value.size)

        def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
            def run(s: S) = {
                value(i) = a
                ((), s)
            }
        }

        def read(i: Int): ST[S, A] = ST(value(i))

        def freeze: ST[S, List[A]] = ST(value.toList)

    }

    object STArray {
        def apply[S, A: Manifest](sz: Int, v: A): STArray[S, A] =
            new STArray[S, A] {
                lazy val value = Array.fill(sz)(v)
            }
    }

    trait RunnableST[A] { def apply[S]: ST[S,A]
    }

    def main(args: Array[String]): Unit = {
        println("Hello ST")

        for {
            r1 <- STRef[Nothing, Int](1)
            r2 <- STRef[Nothing, Int](1)
            x <- r1.read
            y <- r2.read
            _ <- r1.write(y + 1)
            _ <- r2.write(x + 1)
            a <- r1.read
            b <- r2.read
        } yield (a, b)

    }
}
