/// Red-Black Trees
import Debug "mo:base/Debug";
import I "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import O "mo:base/Order";

module {
    /// Node color: red or black.
    public type Color = { #R; #B };

    /// Ordered, (red-black) tree of entries.
    public type Tree<X, Y> = {
        #node : (Color, Tree<X, Y>, (X, ?Y), Tree<X, Y>);
        #leaf;
    };

    type IterRep<X, Y> = List.List<{ #tr:Tree<X, Y>; #xy:(X, ?Y) }>;

    public func empty<X, Y>() : Tree<X, Y> {
        return (#leaf : Tree<X, Y>);
    };
    
    /// An iterator for the entries of the map, in ascending (`#fwd`) or descending (`#bwd`) order.
    public func iter<X, Y>(t : Tree<X, Y>, dir : { #fwd; #bwd }) : I.Iter<(X, Y)> {
        object {
            var trees : IterRep<X, Y> = ?(#tr(t), null);
            public func next() : ?(X, Y) {
                switch (dir, trees) {
                case (_, null) { null };
                case (_, ?(#tr(#leaf), ts)){
                    trees := ts;
                    next()
                };
                case (_, ?(#xy(xy), ts)) {
                    trees := ts;
                    switch (xy.1) {
                    case null { next() };
                    case (?y) { ?(xy.0, y) }
                    }
                };
                case (#fwd, ?(#tr(#node(_, l, xy, r)), ts)) {
                    trees := ?(#tr(l), ?(#xy(xy), ?(#tr(r), ts)));
                    next()
                };
                case (#bwd, ?(#tr(#node(_, l, xy, r)), ts)) {
                    trees := ?(#tr(r), ?(#xy(xy), ?(#tr(l), ts)));
                    next()
                };
                }
            };
            }
    };

    /// Remove the value associated with a given key.
    public func remove<X, Y>(x : X, compareTo : (X, X) -> O.Order, t : Tree<X, Y>) : (?Y, Tree<X, Y>) {
        switch t {
        case (#leaf) { (null, #leaf) };
        case (#node(c, l, xy, r)) {
            switch (compareTo(x, xy.0)) {
            case (#less) {
                let (yo, l2) = remove(x, compareTo, l);
                (yo, #node(c, l2, xy, r))
            };
            case (#equal) {
                (xy.1, #node(c, l, (x, null), r))
            };
            case (#greater) {
                let (yo, r2) = remove(x, compareTo, r);
                (yo, #node(c, l, xy, r2))
            };
            }
        }
        }
    };

    public func put<X, Y>(x : X, compareTo : (X, X) -> O.Order, y : Y, t : Tree<X, Y>) : (?Y, Tree<X, Y>) {
        switch (insertRec(x, compareTo, y, t)) {
            case (_, #leaf) { assert false; loop { } };
            case (yo, #node(_, l, xy, r)) { (yo, #node(#B, l, xy, r)) };
        }
    };

    public func get<X, Y>(x : X, compareTo : (X, X) -> O.Order, t : Tree<X, Y>) : ?Y {
        switch t {
            case (#leaf) { null };
            case (#node(c, l, xy, r)) {
            switch (compareTo(x, xy.0)) {
                case (#less) { get(x, compareTo, l) };
                case (#equal) { xy.1 };
                case (#greater) { get(x, compareTo, r) };
            }
            };
        }
    };

    public type TakeDirection = {
        #asc;
        #dsc;
    };

    // TODO, what is a better name for this?
    public type OrderedFilter<X> = (X) -> O.Order;

    public type TakeResult<X, Y> = {
        result : List.List<(X, Y)>;
        steps  : Nat; // For debugging
    };

    public func treeMin<X, Y>(t0 : Tree<X, Y>) : ?(X, Y) {
        var out : ?(X, Y) = null;
        func traverse(t : Tree<X, Y>) {
            switch(t) {
                case (#leaf)   {};
                case (#node(c, left, (key, valueMaybe), right)) {
                    switch(valueMaybe) {
                        case null {};
                        case (?hasValue) {out := ?(key, hasValue)};
                    };
                    traverse(left);
                }
            };
        };

        traverse(t0);
        return out;
    };

    public func treeMax<X, Y>(t0 : Tree<X, Y>) : ?(X, Y) {
        var out : ?(X, Y) = null;
        func traverse(t : Tree<X, Y>) {
            switch(t) {
                case (#leaf)   {};
                case (#node(c, left, (key, valueMaybe), right)) {
                    switch(valueMaybe) {
                        case null {};
                        case (?hasValue) {out := ?(key, hasValue)};
                    };
                    traverse(right);
                }
            };
        };

        traverse(t0);
        return out;
    };

    // TODO can we leverage Morris Traversal
    public func take<X, Y>(compareTo : (X) -> O.Order, dir : TakeDirection, t0 : Tree<X, Y>) : TakeResult<X, Y> {
        var out = List.nil<(X, Y)>();
        var steps = 0;

        func traverse(t : Tree<X, Y>, compareTo : (X) -> O.Order) {
            steps += 1;
            switch(t) {
                case (#leaf)   {};
                case (#node(c, left, (key, valueMaybe), right)) {
                    switch(compareTo(key), valueMaybe, dir) {
                        case(#equal, ?hasValue, #asc) {
                            traverse(right, compareTo);
                            out := List.push((key, hasValue), out);
                            traverse(left, compareTo);
                        };
                        case(#equal, ?hasValue, #dsc) {
                            traverse(left, compareTo);
                            out := List.push((key, hasValue), out);
                            traverse(right, compareTo);
                        };
                        // Case where we have a key, but no associated value.
                        case(#equal, null, _) {}; 
                        case (#greater, _, _) {
                            traverse(left, compareTo);
                        };
                        case (#less, _, _) {
                            traverse(right, compareTo);
                        };
                    };
                }
            };
        };

        traverse(t0, compareTo);
        return {
            result = out;
            steps  = steps;
        };
    };

    public func height<X, Y>(t : Tree<X, Y>) : Nat {
        switch t {
            case (#leaf) { 0 };
            case (#node(_, l, _, r)) {
            Nat.max(height(l), height(r)) + 1
            }
        }
    };

    /// The size of the tree as the number of key-value entries.
    public func size<X, Y>(t : Tree<X, Y>) : Nat {
        switch t {
            case (#leaf) { 0 };
            case (#node(_, l, _, r)) {
            size(l) + size(r) + 1
            };
        }
    };

    func bal<X, Y>(color : Color, lt : Tree<X, Y>, kv : (X, ?Y), rt : Tree<X, Y>) : Tree<X, Y> {
        // thank you, algebraic pattern matching!
        // following notes from [Ravi Chugh](https://www.classes.cs.uchicago.edu/archive/2019/spring/22300-1/lectures/RedBlackTrees/index.html)
        switch (color, lt, kv, rt) {
            case (#B, #node(#R, #node(#R, a, x, b), y, c), z, d) {
            #node(#R, #node(#B, a, x, b), y, #node(#B, c, z, d))
            };
            case (#B, #node(#R, a, x, #node(#R, b, y, c)), z, d) {
            #node(#R, #node(#B, a, x, b), y, #node(#B, c, z, d))
            };
            case (#B, a, x, #node(#R, #node(#R, b, y, c), z, d)) {
            #node(#R, #node(#B, a, x, b), y, #node(#B, c, z, d))
            };
            case (#B, a, x, #node(#R, b, y, #node(#R, c, z, d))) {
            #node(#R, #node(#B, a, x, b), y, #node(#B, c, z, d))
            };
            case _ { #node(color, lt, kv, rt) };
        }
    };

    func insertRec<X, Y>(x : X, compareTo : (X, X) -> O.Order, y : Y, t : Tree<X, Y>) : (?Y, Tree<X, Y>) {
        switch t {
            case (#leaf) { (null, #node(#R, #leaf, (x, ?y), #leaf)) };
            case (#node(c, l, xy, r)) {
                switch (compareTo(x, xy.0)) {
                    case (#less) {
                    let (yo, l2) = insertRec(x, compareTo, y, l);
                    (yo, bal(c, l2, xy, r))
                    };
                    case (#equal) {
                    (xy.1, #node(c, l, (x, ?y), r))
                    };
                    case (#greater) {
                    let (yo, r2) = insertRec(x, compareTo, y, r);
                    (yo, bal(c, l, xy, r2))
                    };
                }
            }
        }
    };
}
