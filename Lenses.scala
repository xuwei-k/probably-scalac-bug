package com.example

object Lenses {

  type Mutation[C] = C => C

  trait Lens[Container, A] {

    def get(c: Container): A

    def set(a: A): Mutation[Container]

    def modify(f: A => A): Mutation[Container] = c => set(f(get(c)))(c)
  }

  object Lens {
    def apply[Container, A](getter: Container => A)(setter: (Container, A) => Container): Lens[Container, A] = new Lens[Container, A] {
      def get(c: Container) = getter(c)

      def set(a: A): Mutation[Container] = setter(_, a)
    }

    def unit[U]: Lens[U, U] = Lens(identity[U])((c, v) => v)

    class OptLens[U, A](val lens: Lens[U, Option[A]]) extends AnyVal {
      def inplaceMap(f: Lens[A, A] => Mutation[A]) =
        lens.modify(opt => opt.map {
          (m: A) =>
            val field: Lens[A, A] = Lens.unit[A]
            val p: Mutation[A] = f(field)
            p(m)
        })
    }
  }

}

