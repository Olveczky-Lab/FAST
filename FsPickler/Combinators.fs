﻿namespace Nessos.FsPickler

    open System
    open System.IO

    open Nessos.FsPickler.PicklerUtils
    open Nessos.FsPickler.BasePicklers
    open Nessos.FsPickler.DotNetPicklers
    open Nessos.FsPickler.ArrayPickler
    open Nessos.FsPickler.TuplePicklers
    open Nessos.FsPickler.CombinatorImpls

    module Combinators =

        let private defaultSerializer = lazy(new FsPickler())

        /// pickles a value
        let pickle (pickler : Pickler<'T>) (value : 'T) : byte [] =
            defaultSerializer.Value.Pickle (pickler, value)

        /// upickles a value
        let unpickle (pickler : Pickler<'T>) (data : byte []) =
            defaultSerializer.Value.UnPickle (pickler, data)

        /// computes the size of given value
        let getSize (value : 'T) =
            defaultSerializer.Value.ComputeSize(value)

        /// computes the hashcode of given value
        let getHashCode (value : 'T) =
            defaultSerializer.Value.ComputeHash(value)

        [<RequireQualifiedAccess>]
        module Pickler =

            // .NET primitive picklers

            let unit = mkUnitP ()
            let bool = mkBoolP ()
            let byte = mkByteP ()
            let sbyte = mkSByteP ()
            let char = mkCharP ()
            let decimal = mkDecimalP ()
            let single = mkSingleP ()
            let float = mkFloatP ()
            let int16 = mkInt16P ()
            let int = mkInt32P ()
            let int64 = mkInt64P()
            let uint16 = mkUInt16P ()
            let uint32 = mkUInt32P ()
            let uint64 = mkUInt64P ()

            // misc atomic picklers
            let string = mkStringPickler ()
            let guid = mkGuidPickler ()
            let bytes = mkByteArrayPickler ()
            let bigint = mkBigIntPickler () : Pickler<bigint>

            /// the default System.Object pickler
            let obj = mkObjPickler () : Pickler<obj>

            /// auto generate a pickler
            let auto<'T> = defaultSerializer.Value.GeneratePickler<'T> ()

            let inline private uc (p : Pickler<'T>) = p :> Pickler

            /// pair pickler combinator
            let pair f g = Tuple2Pickler.Create(f,g) |> setPicklerId [uc f; uc g]
            /// triple pickler combinator
            let triple f g h = Tuple3Pickler.Create(f,g,h) |> setPicklerId [uc f; uc g; uc h]
            /// quad pickler combinator
            let quad f g h i = Tuple4Pickler.Create(f,g,h,i) |> setPicklerId [uc f; uc g; uc h; uc i]
            /// option pickler combinator
            let option f = OptionPickler.Create f |> setPicklerId [uc f]
            /// Choice<_,_> pickler combinator
            let choice2 f g = Choice2Pickler.Create(f,g) |> setPicklerId [uc f; uc g]
            /// Choice<_,_,_> pickler combinator
            let choice3 f g h = Choice3Pickler.Create(f,g,h) |> setPicklerId [uc f; uc g; uc h]
            /// Choice<_,_,_,_> pickler combinator
            let choice4 f g h i = Choice4Pickler.Create(f,g,h,i) |> setPicklerId [uc f; uc g; uc h; uc i]

            /// FSharp ref pickler combinator
            let ref f = FSharpRefPickler.Create f |> setPicklerId [uc f]
            /// FSharp list pickler combinator
            let list f = ListPickler.Create f |> setPicklerId [uc f]
            /// FSharp map pickler combinator
            let map kp vp = FSharpMapPickler.Create(kp,vp) |> setPicklerId [uc kp; uc kp]
            /// FSharp set pickler combinator
            let set f = FSharpSetPickler.Create f |> setPicklerId [uc f]
            /// array pickler combinator
            let array f = ArrayPickler.Create<'T, 'T []> f |> setPicklerId [uc f]
            /// array2D pickler combinator
            let array2D f = ArrayPickler.Create<'T, 'T [,]> f |> setPicklerId [uc f]
            /// array3D pickler combinator
            let array3D f = ArrayPickler.Create<'T, 'T [,,]> f |> setPicklerId [uc f]
            /// array4D pickler combinator
            let array4D f = ArrayPickler.Create<'T, 'T [,,,]> f |> setPicklerId [uc f]
            /// sequence pickler combinator ; uses eager evaluation
            let seq f = SeqPickler.Create f |> setPicklerId [uc f]
            /// sequence of pairs pickler combinator ; uses eager evaluation
            let pairSeq kp vp = KeyValueSeqPickler.Create(kp, vp) |> setPicklerId [uc kp; uc vp]

            /// wrap combinator: defines picklers up to isomorphism
            let wrap recover convert p = WrapPickler.Create(p, recover, convert) |> setPicklerId [p]
            /// alt combinator: choose from list of pickler combinators using tag reader
            let alt tagReader ps = AltPickler.Create(tagReader, ps) |> setPicklerId (ps |> Seq.map uc)

            /// F# function combinator
            let func<'T, 'U> = AbstractPickler.Create<'T -> 'U> ()

            /// pickler fixpoint combinator
            let fix (F : Pickler<'T> -> Pickler<'T>) =
                let f = new Pickler<'T>()
                let f' = F f
                f.InitializeFrom f' ; f

            /// pickler fixpoint combinator
            let fix2 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'T> * Pickler<'S>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let f',g' = F f g
                f.InitializeFrom f' ; g.InitializeFrom g'
                f,g

            /// pickler fixpoint combinator
            let fix3 (F : Pickler<'T> -> Pickler<'S> -> Pickler<'U> -> Pickler<'T> * Pickler<'S> * Pickler<'U>) =
                let f = new Pickler<'T>()
                let g = new Pickler<'S>()
                let h = new Pickler<'U>()
                let f',g',h' = F f g h
                f.InitializeFrom f' ; g.InitializeFrom g' ; h.InitializeFrom h'
                f,g,h

            /// Experimental support for n-way product types such as records.
            /// See `product` and `field` combinators.
            module ProductInternals =

                /// Internal type for type-checking intermediate values.
                type Part<'R,'X,'Z> =
                    private
                    | P of ('R -> 'Z) * ('X -> 'Z -> 'R) * Pickler<'Z>

                let private pp f g t =
                    P (f, g, t)

                let private finish () =
                    pp ignore (fun r () -> r) unit

                /// Internal type for type-checking intermediate values.
                type Wrap<'T> =
                    internal
                    | W of 'T

                    /// Defines an extra field.
                    static member ( ^+ ) (W f, x) =
                        f x

                    /// Defines the last field.
                    static member ( ^. ) (W f, W x) =
                        f (x (finish ()))

                let internal defProduct e p =
                    match p with
                    | P (f, g, t) ->
                        wrap (g e) f t

                let internal defField proj tf p =
                    match p with
                    | P (g, h, tr) ->
                        pp
                            (fun rr -> (proj rr, g rr))
                            (fun c fx -> h (c (fst fx)) (snd fx))
                            (pair tf tr)

            /// Starts defining a pickler for an n-ary product, such as
            /// record. Example:
            ///
            ///    type Person =
            ///        {
            ///            Address : string
            ///            Age : int
            ///            Name : string
            ///        }
            ///
            ///    let makePerson name age address =
            ///        {
            ///            Address = address
            ///            Age = age
            ///            Name = name
            ///        }
            ///
            ///    let personPickler =
            ///        Pickler.product makePerson
            ///        ^+ Pickler.field (fun p -> p.Name) Pickler.string
            ///        ^+ Pickler.field (fun p -> p.Age) Pickler.int
            ///        ^. Pickler.field (fun p -> p.Address) Pickler.string
            ///
            /// The implementation is not currently efficient, though it
            /// may improve in the future.
            let product f =
                ProductInternals.W (ProductInternals.defProduct f)

            /// See `product`.
            let field f p =
                ProductInternals.W (ProductInternals.defField f p)

            /// Experimental support for n-way sum types such as unions.
            /// See `sum`.
            module SumInternals =

                /// Internal type for type-checking intermediate values.
                type Part<'U,'T,'X,'Y> =
                    private
                    | P of Pickler<'X> * ('X -> 'U) * (('X -> 'Y) -> ('T -> 'Y))

                let private defP p f g =
                    P (p, f, g)

                let private defLastCase inj p =
                    defP p inj (fun h t -> t h)

                let private defNextCase inj p (P (tr, xu, f)) =
                    defP (choice2 p tr)
                        (function
                            | Choice1Of2 x -> inj x
                            | Choice2Of2 x -> xu x)
                        (fun g h ->
                            f (fun x -> g (Choice2Of2 x))
                                (h (fun x -> g (Choice1Of2 x))))

                let private defSum ev (P (tr, xu, f)) =
                    wrap xu (fun u -> f (fun x -> x) (ev u)) tr

                /// Internal type for type-checking intermediate values.
                type Case<'T1,'T2> =
                    internal
                    | C of 'T1 * 'T2

                    /// Adds a case.
                    static member ( ^+ ) (C (i1, p1), W x) =
                        W (defNextCase i1 p1 x)

                    /// Adds the last case.
                    static member ( ^. ) (C (i1, p1), C (i2, p2)) =
                        W (defNextCase i1 p1 (defLastCase i2 p2))

                /// Internal type for type-checking intermediate values.
                and Wrap<'T> =
                    internal
                    | W of 'T

                    /// Adds a case.
                    static member ( ^+ ) (W f, W x) =
                        f x

                    /// Adds the last case.
                    static member ( ^. ) (W f, C (inj, p)) =
                        f (defLastCase inj p)

                let internal makeCase inj p =
                    C (inj, p)

                let internal makeSum f =
                    W (defSum f)

            /// Starts defining a pickler for an n-ary sum type, such as
            /// a union type. For example:
            ///
            ///    type UnionT =
            ///        | Case1
            ///        | Case2 of int
            ///        | Case3 of string * int
            ///
            ///    let unionTPickler =
            ///        Pickler.sum (fun x k1 k2 k3 ->
            ///            match x with
            ///            | Case1 -> k1 ()
            ///            | Case2 x -> k2 x
            ///            | Case3 (x, y) -> k3 (x, y))
            ///        ^+ Pickler.variant Case1
            ///        ^+ Pickler.case Case2 Pickler.int
            ///        ^. Pickler.case Case3 (Pickler.pair Pickler.string Pickler.int)
            ///
            /// Note that the implementation is not currently efficient,
            /// though it may improve in the future.
            let sum f =
                SumInternals.makeSum f

            /// See `sum`.
            let case inj p =
                SumInternals.makeCase inj p

            /// Useful for union cases without arguments.
            let variant v =
                case (fun () -> v) unit
