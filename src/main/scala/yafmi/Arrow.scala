package yafmi

import scalaz.{Monad, ~>}

sealed trait Arrow[A, F[_], B] {
  final def thenArrow[C](arrow: Arrow[B, F, C]): Arrow[A, F, C] = Arrow.Sequence(this, arrow)

  protected def prepared: Arrow[A, F, B]

  def run(fa: F[A])(implicit F: Monad[F]): F[B]

  def mapK[G[_]](nt: F ~> G): Arrow[A, G, B]
}

object Arrow {

  final case class Identity[F[_], A]() extends Arrow[A, F, A] {
    override def prepared: Arrow[A, F, A] = this

    override def run(fa: F[A])(implicit F: Monad[F]): F[A] = fa

    override def mapK[G[_]](nt: F ~> G): Arrow[A, G, A] = Identity()
  }

  final case class Apply[A, F[_], B](f: Free[F, A => B]) extends Arrow[A, F, B] {
    override def prepared: Arrow[A, F, B] = this

    override def run(fa: F[A])(implicit F: Monad[F]): F[B] = f match {
      case Free.Pure(ff) => F.map(fa)(ff)
      case Free.Impure(fi, arrow) => F.ap(fa)(arrow.run(fi))
    }

    override def mapK[G[_]](nt: F ~> G): Arrow[A, G, B] = Apply(f.mapK(nt))
  }

  final case class Bind[A, F[_], B](f: A => Free[F, B]) extends Arrow[A, F, B] {
    override def prepared: Arrow[A, F, B] = this

    override def run(fa: F[A])(implicit F: Monad[F]): F[B] =
      F.bind(fa)(f.andThen(_.run))

    override def mapK[G[_]](nt: F ~> G): Arrow[A, G, B] = BindThenTransform(f, nt)
  }

  final case class BindThenTransform[A, F[_], G[_], B](f: A => Free[F, B], nt: F ~> G) extends Arrow[A, G, B] {
    override protected def prepared: Arrow[A, G, B] = ???

    override def run(fa: G[A])(implicit F: Monad[G]): G[B] =
      F.bind(fa)(f.andThen(free => free.runAs(nt)))

    override def mapK[H[_]](nt2: G ~> H): Arrow[A, H, B] = BindThenTransform(f, nt.andThen(nt2))
  }

  final case class Sequence[A, X, F[_], B](first: Arrow[A, F, X], last: Arrow[X, F, B]) extends Arrow[A, F, B] {
    override def prepared: Arrow[A, F, B] = {
      @scala.annotation.tailrec
      def f[P, Q, R, S](sequence: Sequence[P, Q, F, R], arrow: Arrow[R, F, S]): Arrow[P, F, S] = {
        sequence.first match {
          case sq: Sequence[P, _, F, Q] =>
            f(sq, sequence.last.thenArrow(arrow))
          case _ => Sequence(sequence.first, sequence.last.thenArrow(arrow))
        }
      }

      first match {
        case sequence: Sequence[A, _, F, X] =>
          f(sequence, last)
        case _ => this
      }
    }

    override def run(fa: F[A])(implicit F: Monad[F]): F[B] = {
      @scala.annotation.tailrec
      def f[P, Q, R](fp: F[P], a1: Arrow[P, F, Q], a2: Arrow[Q, F, R]): F[R] = a1 match {
        case Sequence(fa, fl) => f(fp, fa.prepared, fl.thenArrow(a2))
        case _ => a2.prepared match {
          case Sequence(fa, fl) =>
            f(a1.run(fp), fa, fl)
          case _ => a2.run(a1.run(fp))
        }
      }

      f(fa, first.prepared, last.prepared)
    }

    override def mapK[G[_]](nt: F ~> G): Arrow[A, G, B] = {
      @scala.annotation.tailrec
      def f[P, Q, R, S](a1: Arrow[Q, F, R], a2: Arrow[R, F, S], acc: Arrow[P, G, Q]): Arrow[P, G, S] = a1 match {
        case Sequence(s1, s2) => f(s1, s2.thenArrow(a2), acc)
        case _ => a2 match {
          case Sequence(s1, s2) =>
            val x = a1.mapK(nt)
            val y = acc.thenArrow(x)
            f(s1, s2, y)
          case _ =>
            acc.thenArrow(a1.mapK(nt)).thenArrow(a2.mapK(nt))
        }
      }

      first.prepared match {
        case Sequence(s1, s2) => f(s2, last, s1.mapK(nt))
        case _ => last.prepared match {
          case Sequence(s1, s2) => f(s1, s2, first.mapK(nt))
          case _ => first.mapK(nt).thenArrow(last.mapK(nt))
        }
      }
    }
  }

  def applied[A, F[_], B](f: Free[F, A => B]): Arrow[A, F, B] = Apply(f)

  def bind[A, F[_], B](f: A => Free[F, B]): Arrow[A, F, B] = Bind(f)

  def identity[F[_], A]: Arrow[A, F, A] = Identity()
}
