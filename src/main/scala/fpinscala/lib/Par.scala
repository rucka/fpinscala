object lib_par {

  object Par {
    import java.util.concurrent._
    import language.implicitConversions

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
      }

    def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def map[A,B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a,_) => f(a))

    def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

    def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
      l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

    // This implementation forks the recursive step off to a new logical thread,
    // making it effectively tail-recursive. However, we are constructing
    // a right-nested parallel program, and we can get better performance by
    // dividing the list in half, and running both halves in parallel.
    // See `sequenceBalanced` below.
    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
      }

    // We define `sequenceBalanced` using `IndexedSeq`, which provides an
    // efficient function for splitting the sequence in half.
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map (asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
        else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => {
        val ind = run(es)(n).get // Full source files
        run(es)(choices(ind))
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
      es => {
        val k = run(es)(key).get
        run(es)(choices(k))
      }

    def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = run(es)(p).get
        run(es)(choices(k))
      }

    /* `chooser` is usually called `flatMap` or `bind`. */
    def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = run(es)(p).get
        run(es)(choices(k))
      }

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    // see nonblocking implementation in `Nonblocking.scala`
    def join[A](a: Par[Par[A]]): Par[A] =
      es => run(es)(run(es)(a).get())

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))
    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {

    }
  }
  object Nonblocking {
    import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
    import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
    import language.implicitConversions

    import annotation.tailrec
    
    trait Future[+A] {
      def apply(k: A => Unit): Unit
    }

    type Par[+A] = ExecutorService => Future[A]

    object Par {
      def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
        val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
        p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
        latch.await // Block until the `latch.countDown` is invoked asynchronously
        ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
      }

      def unit[A](a: A): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            cb(a)
        }

      /** A non-strict version of `unit` */
      def delay[A](a: => A): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            cb(a)
        }

      def fork[A](a: => Par[A]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            eval(es)(a(es)(cb))
        }

      /**
       * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
       * This will come in handy in Chapter 13.
       */
      def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
        def apply(k: A => Unit) = f(k)
      }

      /**
       * Helper function, for evaluating an action
       * asynchronously, using the given `ExecutorService`.
       */
      def eval(es: ExecutorService)(r: => Unit): Unit =
        es.submit(new Callable[Unit] { def call = r })

      def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
        es => new Future[C] {
          def apply(cb: C => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None
            // this implementation is a little too liberal in forking of threads -
            // it forks a new logical thread for the actor and for stack-safety,
            // forks evaluation of the callback `cb`
            val combiner = Actor[Either[A,B]](es) {
              case Left(a) =>
                if (br.isDefined) eval(es)(cb(f(a,br.get)))
                else ar = Some(a)
              case Right(b) =>
                if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
                else br = Some(b)
            }
            p(es)(a => combiner ! Left(a))
            p2(es)(b => combiner ! Right(b))
          }
        }

      // specialized version of `map`
      def map[A,B](p: Par[A])(f: A => B): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => eval(es) { cb(f(a)) })
        }

      def lazyUnit[A](a: => A): Par[A] =
        fork(unit(a))

      def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

      def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
        as match {
          case Nil => unit(Nil)
          case h :: t => map2(h, fork(sequence(t)))(_ :: _)
        }

      def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
        if (as.isEmpty) unit(Vector())
        else if (as.length == 1) map(as.head)(a => Vector(a))
        else {
          val (l,r) = as.splitAt(as.length/2)
          map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
        }
      }

      def sequence[A](as: List[Par[A]]): Par[List[A]] =
        map(sequenceBalanced(as.toIndexedSeq))(_.toList)

      def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
        sequence(as.map(asyncF(f)))

      def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
        sequenceBalanced(as.map(asyncF(f)))

      // exercise answers

      /*
       * We can implement `choice` as a new primitive.
       *
       * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
       * some `Par`, `p`, is the idiom for running `p`, and registering
       * a callback to be invoked when its result is available. The
       * result will be bound to `result` in the function passed to
       * `p(es)`.
       *
       * If you find this code difficult to follow, you may want to
       * write down the type of each subexpression and follow the types
       * through the implementation. What is the type of `p(es)`? What
       * about `t(es)`? What about `t(es)(cb)`?
       */
      def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es) { b =>
              if (b) eval(es) { t(es)(cb) }
              else eval(es) { f(es)(cb) }
            }
        }

      /* The code here is very similar. */
      def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es) { ind => eval(es) { ps(ind)(es)(cb) }}
        }

      def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
        choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

      def choiceMap[K,V](p: Par[K])(ps: Map[K,Par[V]]): Par[V] =
        es => new Future[V] {
          def apply(cb: V => Unit): Unit =
            p(es)(k => ps(k)(es)(cb))
        }

      /* `chooser` is usually called `flatMap` or `bind`. */
      def chooser[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        flatMap(p)(f)

      def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        es => new Future[B] {
          def apply(cb: B => Unit): Unit =
            p(es)(a => f(a)(es)(cb))
        }

      def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
        flatMap(p)(b => if (b) t else f)

      def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
        flatMap(p)(i => choices(i))

      def join[A](p: Par[Par[A]]): Par[A] =
        es => new Future[A] {
          def apply(cb: A => Unit): Unit =
            p(es)(p2 => eval(es) { p2(es)(cb) })
        }

      def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
        flatMap(a)(x => x)

      def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
        join(map(p)(f))

      /* Gives us infix syntax for `Par`. */
      implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

      // infix versions of `map`, `map2` and `flatMap`
      class ParOps[A](p: Par[A]) {
        def map[B](f: A => B): Par[B] = Par.map(p)(f)
        def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
        def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
        def zip[B](b: Par[B]): Par[(A,B)] = p.map2(b)((_,_))
      }
    }

    /*
     * Implementation is taken from `scalaz` library, with only minor changes. See:
     *
     * https://github.com/scalaz/scalaz/blob/scalaz-seven/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
     *
     * This code is copyright Andriy Plokhotnyuk, Runar Bjarnason, and other contributors,
     * and is licensed using 3-clause BSD, see LICENSE file at:
     *
     * https://github.com/scalaz/scalaz/blob/scalaz-seven/etc/LICENCE
     */

    /**
     * Processes messages of type `A`, one at a time. Messages are submitted to
     * the actor with the method `!`. Processing is typically performed asynchronously,
     * this is controlled by the provided `strategy`.
     *
     * Memory consistency guarantee: when each message is processed by the `handler`, any memory that it
     * mutates is guaranteed to be visible by the `handler` when it processes the next message, even if
     * the `strategy` runs the invocations of `handler` on separate threads. This is achieved because
     * the `Actor` reads a volatile memory location before entering its event loop, and writes to the same
     * location before suspending.
     *
     * Implementation based on non-intrusive MPSC node-based queue, described by Dmitriy Vyukov:
     * [[http://www.1024cores.net/home/lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue]]
     *
     * @see scalaz.concurrent.Promise for a use case.
     *
     * @param handler  The message handler
     * @param onError  Exception handler, called if the message handler throws any `Throwable`.
     * @param strategy Execution strategy, for example, a strategy that is backed by an `ExecutorService`
     * @tparam A       The type of messages accepted by this actor.
     */
    final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
      self =>

      private val tail = new AtomicReference(new Node[A]())
      private val suspended = new AtomicInteger(1)
      private val head = new AtomicReference(tail.get)

      /** Alias for `apply` */
      def !(a: A) {
        val n = new Node(a)
        head.getAndSet(n).lazySet(n)
        trySchedule()
      }

      /** Pass the message `a` to the mailbox of this actor */
      def apply(a: A) {
        this ! a
      }

      def contramap[B](f: B => A): Actor[B] =
        new Actor[B](strategy)((b: B) => (this ! f(b)), onError)

      private def trySchedule() {
        if (suspended.compareAndSet(1, 0)) schedule()
      }

      private def schedule() {
        strategy(act())
      }

      private def act() {
        val t = tail.get
        val n = batchHandle(t, 1024)
        if (n ne t) {
          n.a = null.asInstanceOf[A]
          tail.lazySet(n)
          schedule()
        } else {
          suspended.set(1)
          if (n.get ne null) trySchedule()
        }
      }

      @tailrec
      private def batchHandle(t: Node[A], i: Int): Node[A] = {
        val n = t.get
        if (n ne null) {
          try {
            handler(n.a)
          } catch {
            case ex: Throwable => onError(ex)
          }
          if (i > 0) batchHandle(n, i - 1) else n
        } else t
      }
    }

    private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]]

    object Actor {

      /** Create an `Actor` backed by the given `ExecutorService`. */
      def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] =
        new Actor(Strategy.fromExecutorService(es))(handler, onError)
    }

    /**
     * Provides a function for evaluating expressions, possibly asynchronously.
     * The `apply` function should typically begin evaluating its argument
     * immediately. The returned thunk can be used to block until the resulting `A`
     * is available.
     */
    trait Strategy {
      def apply[A](a: => A): () => A
    }

    object Strategy {

      /**
       * We can create a `Strategy` from any `ExecutorService`. It's a little more
       * convenient than submitting `Callable` objects directly.
       */
      def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
        def apply[A](a: => A): () => A = {
          val f = es.submit { new Callable[A] { def call = a} }
          () => f.get
        }
      }

      /**
       * A `Strategy` which begins executing its argument immediately in the calling thread.
       */
      def sequential: Strategy = new Strategy {
        def apply[A](a: => A): () => A = {
          val r = a
          () => r
        }
      }
    }
  }
}
