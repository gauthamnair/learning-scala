
import scala.language.higherKinds

object ch11 {
	trait Functor[F[_]] {
		def map[A,B](fa: F[A])(f: A => B): F[B]

		def distribute[A,B](ab: F[(A,B)]): (F[A], F[B]) = 
			(map(ab)(_._1), map(ab)(_._2))
	}

	val listFunctor = new Functor[List] {
		def map[A,B](as: List[A])(f: A => B): List[B] =
			as.map(f)
	}

	trait Monad[F[_]] {
		
		def unit[A](a: A): F[A]
		def flatMap[A,B](as: F[A])(f: A => F[B]): F[B]

		def map[A,B](as: F[A])(f: A => B): F[B] = 
			flatMap(as)(a => unit(f(a)))

		def map2[A,B,C](as: F[A], bs: F[B])(f: (A,B) => C): F[C] =
			flatMap(as)(a => map(bs)(b => f(a,b)))

		def sequence[A](listFas: List[F[A]]): F[List[A]] = 
			listFas match {
				case fa :: rest => map2(fa, sequence(rest))(_ :: _)
				case Nil => unit(Nil: List[A])
			}

		object byFolding {
			def sequence[A](listFas: List[F[A]]): F[List[A]] = 
				listFas.foldRight(
					unit(Nil: List[A])
					)((fa, fofList) => map2(fa, fofList)(_ :: _))
		}

		def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = 
			sequence(la.map(f))
	}

	object ex11_1 {
		val listMonad = new Monad[List] {
			def unit[A](a: A) = List(a)
			def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
				as.flatMap(f)
		}
		val optionMonad = new Monad[Option] {
			def unit[A](a: A) = Some(a)
			def flatMap[A,B](as: Option[A])(f: A => Option[B]): Option[B] =
				as.flatMap(f)
		}
	}

	def main(args: Array[String]) {
	}
}