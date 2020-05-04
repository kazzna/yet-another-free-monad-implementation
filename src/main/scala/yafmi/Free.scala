package yafmi

import scalaz.{Monad, ~>}

sealed trait Free[F[_], A] {
  final def map[B](f: A => B): Free[F, B] = flatMap(f.andThen(Free.pure))
  final def ap[B](f: Free[F, A => B]): Free[F, B] = this match {
    case Free.Pure(a) => f.map(_(a))
    case Free.Impure(fi, arrow) => Free.Impure(fi, arrow.thenArrow(Arrow.applied(f)))
  }
  final def map2[B, C](fb: Free[F, B])(f: (A, B) => C): Free[F, C] =
    fb.ap(this.ap(Free.pure(f.curried)))
  final def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
    case Free.Pure(a) => f(a)
    case Free.Impure(fi, arrow) => Free.Impure(fi, arrow.thenArrow(Arrow.bind(f)))
  }

  final def run(implicit F: Monad[F]): F[A] = this match {
    case Free.Pure(a) => F.point(a)
    case Free.Impure(fi, arrow) => arrow.run(fi)
  }

  def runAs[G[_]: Monad](nt: F ~> G): G[A] = transform(nt).run

  def transform[G[_]](nt: F ~> G): Free[G, A] = this match {
    case Free.Pure(a) => Free.pure(a)
    case Free.Impure(fi, arrow) => Free.Impure(nt(fi), arrow.transform(nt))
  }
}

object Free {
  final case class Pure[F[_], A](a: A) extends Free[F, A]
  final case class Impure[I, F[_], A](fi: F[I], arrow: Arrow[I, F, A]) extends Free[F, A]

  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  def point[F[_], A](a: A): Free[F, A] = Pure(a)

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Impure(fa, Arrow.identity)
}
