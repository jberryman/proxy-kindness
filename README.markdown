A small library for kind-polymorphic manipulation and inspection of
[`Proxy`](http://hackage.haskell.org/package/tagged-0.7/docs/Data-Proxy.html)
values. Here are some usage examples:
                     
Force @Left 1@ to be of type @Either Float a@ using a partially-applied
'Proxy'.

    >>> let t = (Left 1) `asApplied` (Proxy :: Proxy (Either Float))
    >>> :t t
    t :: Either Float a

Do the same, but illustrating type application with @ap@

    >>> let t0 = (Left 1) `asApplied` ((Proxy :: Proxy Either) `ap` (Proxy :: Proxy Float))
    >>> :t t0
    t0 :: Either Float a

Force polymorphic @undefined@ to some polymorphic @Either@ type, the base type
of @t@ defined above.

    >>> let u = undefined `asApplied` (unappliedOf t)
    >>> :t u
    u :: Either a a1
