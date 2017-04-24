﻿namespace Nessos.FsPickler
    
    open System
    open System.IO
    open System.Collections
    open System.Collections.Generic
    open System.Runtime.Serialization

    open Nessos.FsPickler.Utils
    open Nessos.FsPickler.Hashing
    open Nessos.FsPickler.PicklerUtils

    type private OAttribute = System.Runtime.InteropServices.OptionalAttribute
    type private DAttribute = System.Runtime.InteropServices.DefaultParameterValueAttribute

    type HashResult =
        {
            Algorithm : string
            Length : int64
            Hash : byte []
        }

    [<Sealed>]
    [<AutoSerializableAttribute(false)>]
    type FsPickler private (cache : PicklerCache) =

        let resolver = cache :> IPicklerResolver
        
        /// initializes an instance that resolves picklers from a global cache
        new () = new FsPickler(PicklerCache.GetDefaultInstance())
        /// initializes a new pickler cache that resolves picklers using custom rules
        new (registry : CustomPicklerRegistry) = new FsPickler(PicklerCache.FromPicklerRegistry registry)

        /// Name for the pickler cache
        member __.Name = cache.Name
        /// Identifier of the cache instance used by the serializer.
        member __.UUId = cache.UUId

        /// <summary>Serialize value to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary writer.</param>
        /// <param name="leaveOpen">Leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(stream : Stream, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            let qn = cache.GetQualifiedName pickler.Type
            writer.WriteRootObject<'T>(pickler, qn, value)

        /// <summary>Serialize value to the underlying stream using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize<'T>(pickler : Pickler<'T>, stream : Stream, value : 'T, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            do checkPicklerCompat cache.UUId pickler
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qn = cache.GetQualifiedName pickler.Type
            writer.WriteRootObject(pickler, qn, value)

        /// <summary>Serialize object of given type to the underlying stream.</summary>
        /// <param name="valueType">type of the given object.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(valueType : Type, stream : Stream, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve valueType
            let qn = cache.GetQualifiedName valueType
            pickler.WriteRootObject (writer, qn, value)

        /// <summary>Serialize object to the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for serialization.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="value">value to be serialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Serialize(pickler : Pickler, stream : Stream, value : obj, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : unit =
            use writer = new Writer(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qn = cache.GetQualifiedName pickler.Type
            pickler.WriteRootObject (writer, qn, value)

        /// <summary>Deserialize value of given type from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            let qn = cache.GetQualifiedName pickler.Type
            reader.ReadRootObject<'T> (pickler, qn)

        /// <summary>Deserialize value of given type from the underlying stream, using given pickler.</summary>
        /// <param name="pickler">pickler used for serialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext"> streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize<'T> (pickler : Pickler<'T>, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : 'T =
            do checkPicklerCompat cache.UUId pickler
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qn = cache.GetQualifiedName pickler.Type
            reader.ReadRootObject<'T> (pickler, qn)

        /// <summary>Deserialize object of given type from the underlying stream.</summary>
        /// <param name="valueType">anticipated value type.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.Deserialize (valueType : Type, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve valueType
            let qn = cache.GetQualifiedName valueType
            pickler.ReadRootObject (reader, qn)

        /// <summary>Deserialize object from the underlying stream using given pickler.</summary>
        /// <param name="pickler">untyped pickler used for deserialization.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.Deserialize (pickler : Pickler, stream : Stream, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : obj =
            use reader = new Reader(stream, resolver, ?streamingContext = streamingContext, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let qn = cache.GetQualifiedName pickler.Type
            pickler.ReadRootObject (reader, qn)

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        member __.SerializeSequence<'T>(stream : Stream, sequence:seq<'T>, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =
            use writer = new Writer(stream, resolver, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            let qn = cache.GetQualifiedName pickler.Type
            writer.WriteSequence(pickler, qn, sequence)

        /// <summary>Serialize a sequence of objects to the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">target stream.</param>
        /// <param name="sequence">input sequence.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <return>number of elements written to the stream.</return>
        member __.SerializeSequence(elementType : Type, stream : Stream, sequence : IEnumerable, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : int =
            use writer = new Writer(stream, resolver, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve elementType
            let qn = cache.GetQualifiedName pickler.Type
            pickler.WriteSequence(writer, qn, sequence)

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence<'T>(stream : Stream, length : int, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator<'T> =
            let reader = new Reader(stream, resolver, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve<'T> ()
            let qn = cache.GetQualifiedName pickler.Type
            reader.ReadSequence(pickler, qn, length)

        /// <summary>Lazily deserialize a sequence of objects from the underlying stream.</summary>
        /// <param name="elementType">element type used in sequence.</param>
        /// <param name="stream">source stream.</param>
        /// <param name="length">number of elements to be deserialized.</param>
        /// <param name="streamingContext">streaming context.</param>
        /// <param name="encoding">encoding passed to the binary reader.</param>
        /// <param name="leaveOpen">leave underlying stream open when finished. Defaults to true.</param>
        /// <returns>An IEnumerator that lazily consumes elements from the stream.</returns>
        member __.DeserializeSequence(elementType : Type, stream : Stream, length : int, [<O;D(null)>]?streamingContext, [<O;D(null)>]?encoding, [<O;D(null)>]?leaveOpen) : IEnumerator =
            let reader = new Reader(stream, resolver, ?encoding = encoding, ?leaveOpen = leaveOpen)
            let pickler = resolver.Resolve elementType
            let qn = cache.GetQualifiedName pickler.Type
            pickler.ReadSequence(reader, qn, length)

        /// creates a byte array pickle out of given pickler and value
        member f.Pickle (pickler : Pickler<'T>, value : 'T) : byte [] =
            pickle (fun m v -> f.Serialize(pickler, m, v)) value

        /// creates a byte array pickle out of a given value
        member f.Pickle (value : 'T) : byte [] =
            pickle (fun m v -> f.Serialize(m, v)) value

        /// deserializes value out of given byte array using given pickler
        member f.UnPickle (pickler : Pickler<'T>, data : byte []) =
            unpickle (fun m -> f.Deserialize(pickler, m)) data

        /// deserializes value out of a given byte array
        member f.UnPickle<'T> (data : byte []) =
            unpickle (fun m -> f.Deserialize<'T> m) data

        /// <summary>Compute size and hashcode for given input.</summary>
        /// <param name="value">input value.</param>
        /// <param name="pickler">use specific pickler for hashcode generation.</param>
        /// <param name="hashFactory">the hashing algorithm to be used. MurMur3 by default</param>
        member f.ComputeHash<'T>(value : 'T, [<O;D(null)>] ?pickler : Pickler<'T>, [<O;D(null)>] ?hashFactory : IHashStreamFactory) =
            let hashStream = 
                match hashFactory with 
                | Some h -> h.Create()
                | None -> new MurMur3Stream() :> HashStream

            match pickler with
            | None -> f.Serialize(hashStream, value)
            | Some p -> f.Serialize(p, hashStream, value)

            {
                Algorithm = hashStream.HashAlgorithm
                Length = hashStream.Length
                Hash = hashStream.ComputeHash()
            }

        /// <summary>Compute size in bytes for given input.</summary>
        /// <param name="pickler">use specific pickler for length computation.</param>
        /// <param name="value">input value.</param>
        member f.ComputeSize<'T>(value : 'T, [<O;D(null)>]?pickler : Pickler<'T>) =
            let lengthCounter = new LengthCounter()
            match pickler with
            | None -> f.Serialize(lengthCounter, value)
            | Some p -> f.Serialize(p, lengthCounter, value)
            lengthCounter.Length

        /// Auto generates a pickler for given type variable
        member __.GeneratePickler<'T> () = resolver.Resolve<'T> ()
        
        /// Auto generates a pickler for given type
        member __.GeneratePickler (t : Type) = resolver.Resolve t


        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType (t : Type) =
            try resolver.Resolve t |> ignore ; true
            with :? NonSerializableTypeException -> false

        /// Decides if given type is serializable by FsPickler
        member __.IsSerializableType<'T> () =
            try resolver.Resolve<'T> () |> ignore ; true
            with :? NonSerializableTypeException -> false