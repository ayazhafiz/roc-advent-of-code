# Taken from Brendan. Thanks Brendan!
# https://github.com/bhansconnect/roc-dict/tree/main/examples

interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove, clear, capacity, len, walkValues ]
    imports [ Wyhash ]

# This is based off of absl::flat_hash_map.
# It is simplified to make it nicer to write in roc.
emptySlot : I8
emptySlot = -128
deletedSlot : I8
deletedSlot = -2
# sentinel : I8
# sentinel = -1
Elem a : [ T U64 a ]

# Metadata should be grouped. This helps make loading faster.
# One memory load would then load 8 possible indexes.
# These could then be compared with vector instructions (if added to roc).
# To start just making it non-grouped for simplicity.
U64FlatHashDict a := {
        data : List (Elem a),
        metadata : List I8,
        size : Nat,
        default : Elem a,
        seed : Wyhash.Seed,
    }

# Capacity must be a power of 2.
# We still will use slots of 8 even though this version has no true slots.
# We just move an element at a time.
# Thus, the true index is slotIndex * 8 + offset.
Probe : { slotIndex: Nat, probeI: Nat, mask: Nat }

newProbe : U64, Nat -> Probe
newProbe = \h1Key, slots ->
    mask = Num.subSaturated slots 1
    slotIndex = Num.bitwiseAnd (Num.toNat h1Key) mask
    { slotIndex, probeI: 1, mask }

nextProbe : Probe -> Probe
nextProbe = \{ slotIndex, probeI, mask } ->
    nextSlotIndex = Num.bitwiseAnd (Num.addWrap slotIndex probeI) mask
    { slotIndex: nextSlotIndex, probeI: Num.addWrap probeI 1, mask }

mul8 = \val -> Num.shiftLeftBy val 3
div8 = \val -> Num.shiftRightZfBy val 3

# This requires an element because we don't know how to generate a default elem.
# For simplicity for now, I am just storing the default value.
empty : a -> U64FlatHashDict a
empty = \default ->
    defaultElem = T 0 default

    @U64FlatHashDict
        {
            data: List.repeat defaultElem 8,
            metadata: List.repeat emptySlot 8,
            size: 0,
            default: defaultElem,
            seed: Wyhash.createSeed 0x0123_4567_89AB_CDEF,
        }

len : U64FlatHashDict a -> Nat
len = \@U64FlatHashDict { size } ->
    size

capacity : U64FlatHashDict a -> Nat
capacity = \@U64FlatHashDict { data } ->
    List.len data

clear : U64FlatHashDict a -> U64FlatHashDict a
clear = \@U64FlatHashDict { data, metadata, default, seed } ->
    cap = List.len data
    # Only clear large allocations.
    if cap > 128 * 8 then
        when default is
            T _ v ->
                empty v
    else
        @U64FlatHashDict {
            data: List.map data (\_ -> default),
            metadata: List.map metadata (\_ -> emptySlot),
            size: 0,
            default,
            seed
        }

contains : U64FlatHashDict a, U64 -> Bool
contains = \@U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata data h2Key key probe 0 is
        Found _ ->
            Bool.true

        NotFound ->
            Bool.false

get : U64FlatHashDict a, U64 -> Result a [ NotFound ]
get = \@U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findValueHelper metadata data h2Key key probe 0 is
        Found _ v ->
            Ok v

        NotFound ->
            Err NotFound

remove : U64FlatHashDict a, U64 -> [ T (U64FlatHashDict a) Bool ]
remove = \@U64FlatHashDict { data, metadata, size, default, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata data h2Key key probe 0 is
        Found index ->
            T (@U64FlatHashDict { data, metadata: List.set metadata index deletedSlot, size: (size - 1), default, seed }) Bool.true

        NotFound ->
            T (@U64FlatHashDict { data, metadata, size, default, seed }) Bool.false

# Does insertion without potentially rehashing.
insert : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insert = \@U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    when findIndexHelper metadata data h2Key key probe 0 is
        Found index ->
            @U64FlatHashDict
                {
                    data: List.set data index (T key value),
                    metadata: List.set metadata index h2Key,
                    size,
                    default,
                    seed,
                }
        NotFound ->
            # Need to rescan searching for the first empty or deleted cell.
            rehashedDict =
                maybeRehash (
                    @U64FlatHashDict
                        {
                            data,
                            metadata,
                            size: size + 1,
                            default,
                            seed,
                        }
                )
            insertNotFoundHelper rehashedDict key value h1Key h2Key

walkValues : U64FlatHashDict a, st, (st, a -> st) -> st
walkValues = \@U64FlatHashDict { data }, st, walker ->
    List.walk data st (\state, T _ val -> walker state val)

# This and the next function are very similar.
# They can not be merged because that will lead to them not getting inlined
# That can have a large performance impact.
insertNotFoundHelper : U64FlatHashDict a, U64, a, U64, I8 -> U64FlatHashDict a
insertNotFoundHelper = \@U64FlatHashDict { data, metadata, size, default, seed }, key, value, h1Key, h2Key ->
    probe = newProbe h1Key (div8 (List.len metadata))
    index = fillEmptyOrDeletedHelper metadata data h2Key key probe 0
    @U64FlatHashDict
        {
            data: List.set data index (T key value),
            metadata: List.set metadata index h2Key,
            size,
            default,
            seed,
        }

insertInEmptyOrDeleted : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInEmptyOrDeleted = \@U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    probe = newProbe h1Key (div8 (List.len metadata))

    index = fillEmptyOrDeletedHelper metadata data h2Key key probe 0
    @U64FlatHashDict
        {
            data: List.set data index (T key value),
            metadata: List.set metadata index h2Key,
            size,
            default,
            seed,
        }

fillEmptyOrDeletedHelper : List I8, List (Elem a), I8, U64, Probe, Nat -> Nat
fillEmptyOrDeletedHelper = \metadata, data, h2Key, key, probe, offset ->
    # For inserting, we can use deleted indices.
    index = Num.addWrap (mul8 probe.slotIndex) offset

    when List.get metadata index is
        Ok md ->
            if md < 0 then
                # Empty or deleted slot no possibility of the element
                index
            else
                # Used slot, check next slot
                if offset == 7 then
                    fillEmptyOrDeletedHelper metadata data h2Key key (nextProbe probe) 0
                else
                    fillEmptyOrDeletedHelper metadata data h2Key key probe (Num.addWrap offset 1)

        Err OutOfBounds ->
            # not possible. just panic
            (0 - 1)

findIndexHelper : List I8, List (Elem a), I8, U64, Probe, Nat -> [ Found Nat, NotFound ]
findIndexHelper = \metadata, data, h2Key, key, probe, offset ->
    # For inserting, we can use deleted indices.
    index = Num.addWrap (mul8 probe.slotIndex) offset

    when List.get metadata index is
        Ok md ->
            if md == emptySlot then
                # Empty slot, no possibility of the element
                NotFound
            else if md == h2Key then
                # This is potentially a match.
                # Check data for if it is a match.
                when List.get data index is
                    Ok (T k _) ->
                        if k == key then
                            # we have a match, return it's index
                            Found index
                        else
                            # no match, keep checking.
                            if offset == 7 then
                                findIndexHelper metadata data h2Key key (nextProbe probe) 0
                            else
                                findIndexHelper metadata data h2Key key probe (Num.addWrap offset 1)

                    Err OutOfBounds ->
                        # not possible. just panic
                        NotFound
            else
                # Used slot, check next slot
                if offset == 7 then
                    findIndexHelper metadata data h2Key key (nextProbe probe) 0
                else
                    findIndexHelper metadata data h2Key key probe (Num.addWrap offset 1)

        Err OutOfBounds ->
            # not possible. just panic
            NotFound

findValueHelper : List I8, List (Elem a), I8, U64, Probe, Nat -> [ Found Nat a, NotFound ]
findValueHelper = \metadata, data, h2Key, key, probe, offset ->
    # For finding we have to scan past deleted items.
    index = Num.addWrap (mul8 probe.slotIndex) offset

    when List.get metadata index is
        Ok md ->
            if md == emptySlot then
                # Empty slot no possibility of the element
                NotFound
            else if md == h2Key then
                # This is potentially a match.
                # Check data for if it is a match.
                when List.get data index is
                    Ok (T k v) ->
                        if k == key then
                            # we have a match, return it's index
                            Found index v
                        else
                            # no match, keep checking.
                            if offset == 7 then
                                findValueHelper metadata data h2Key key (nextProbe probe) 0
                            else
                                findValueHelper metadata data h2Key key probe (Num.addWrap offset 1)


                    Err OutOfBounds ->
                        # not possible. just panic
                        NotFound
            else
                # Used or deleted slot, keep checking.
                if offset == 7 then
                    findValueHelper metadata data h2Key key (nextProbe probe) 0
                else
                    findValueHelper metadata data h2Key key probe (Num.addWrap offset 1)

        Err OutOfBounds ->
            # not possible. just panic
            NotFound

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
maybeRehash : U64FlatHashDict a -> U64FlatHashDict a
maybeRehash = \@U64FlatHashDict { data, metadata, size, default, seed } ->
    cap = List.len data
    maxLoadCap =
            # This is 7/8 * capacity, which is the max load factor.
            cap - (Num.shiftRightZfBy cap 3)
    if size >= maxLoadCap then
        rehash (@U64FlatHashDict { data, metadata, size, default, seed })
    else
        @U64FlatHashDict { data, metadata, size, default, seed }

rehash : U64FlatHashDict a -> U64FlatHashDict a
rehash = \@U64FlatHashDict { data, metadata, size, default, seed } ->
    newLen = 2 * List.len data
    newDict =
        @U64FlatHashDict
            {
                data: List.repeat default newLen,
                metadata: List.repeat emptySlot newLen,
                size,
                default,
                seed,
            }

    rehashHelper newDict metadata data 0

rehashHelper : U64FlatHashDict a, List I8, List (Elem a), Nat -> U64FlatHashDict a
rehashHelper = \dict, metadata, data, index ->
    when List.get metadata index is
        Ok md ->
            nextDict =
                if md >= 0 then
                    # We have an actual element here
                    when List.get data index is
                        Ok (T k v) ->
                            insertInEmptyOrDeleted dict k v

                        Err OutOfBounds ->
                            # This should be an impossible state since data and metadata are the same size
                            dict
                else
                    # Empty or deleted data
                    dict

            rehashHelper nextDict metadata data (index + 1)

        Err OutOfBounds ->
            dict

h1 : U64 -> U64
h1 = \hashKey ->
    Num.shiftRightZfBy hashKey 7

h2 : U64 -> I8
h2 = \hashKey ->
    Num.toI8 (Num.bitwiseAnd hashKey 0b0111_1111)
