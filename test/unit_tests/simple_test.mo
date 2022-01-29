import Debug "mo:base/Debug";
import Iter "mo:base/Iter";
import List "mo:base/List";
import ModuleSpec "../utils/ModuleSpec";
import Nat "mo:base/Nat";
import Optional "mo:base/Option";
import Order "mo:base/Order";
import RBTree "../../src/FunctionalRBTree";

module {
    type Group         = ModuleSpec.Group;
    
    let assertTrue     = ModuleSpec.assertTrue;
    let assertAllTrue  = ModuleSpec.assertAllTrue;
    let describe       = ModuleSpec.describe;
    let it             = ModuleSpec.it;
    let skip           = ModuleSpec.skip;
    let pending        = ModuleSpec.pending;
    let run            = ModuleSpec.run;

    func getFilledTree(min : Nat, max : Nat) : RBTree.Tree<Nat, Nat> {
        var t = RBTree.empty<Nat, Nat>();
        
        for (x in Iter.range(min, max)) {
            t := RBTree.put(x, Nat.compare, x, t).1;
        };

        return t;
    };

    func buildFilter(min : Nat, max : Nat) : RBTree.OrderedFilter<Nat> {
        return func(this : Nat) : Order.Order {            
            if (Nat.greaterOrEqual(this, min) and Nat.lessOrEqual(this, max)) {
                return #equal;
            } else if (Nat.less(this, min)) {
                return #less;
            };
            return #greater;
        };
    };

    func simpleTests() : Group {
        return describe("Simple Tests", [
                it("should put and get a value", do {
                    var tree = RBTree.empty<Nat, Nat>();
                    let (existing, newTree) = RBTree.put(0, Nat.compare, 0, tree);
                    
                    assertAllTrue([
                        Optional.isNull(RBTree.get(0, Nat.compare, tree)), // Original tree does not have value
                        Optional.isSome(RBTree.get(0, Nat.compare, newTree)), // Original tree does not have value
                    ]);
                }),
                it("should return the old value if it exists", do {
                    var tree = RBTree.empty<Nat, Nat>();
                    let (shouldBeNull, treeOne) = RBTree.put(0, Nat.compare, 0, tree);
                    let (shouldBeZero, treeTwo) = RBTree.put(0, Nat.compare, 123, treeOne);

                    assertAllTrue([
                        Optional.isNull(shouldBeNull), // Original tree does not have value
                        Optional.unwrap(shouldBeZero) == 0, // Original tree does not have value
                    ]);
                }),
                it("should remove a value", do {
                    var tree = RBTree.empty<Nat, Nat>();
                    let (_, treeOne) = RBTree.put(0, Nat.compare, 123, tree);
                    let (shouldHaveValue, treeTwo) = RBTree.remove(0, Nat.compare, treeOne);

                    assertAllTrue([
                        Optional.unwrap(RBTree.get(0, Nat.compare, treeOne)) == 123, // Original tree does not have value
                        Optional.isNull(RBTree.get(0, Nat.compare, treeTwo)), // Original tree does not have value
                        Optional.unwrap(shouldHaveValue) == 123, // Original tree does not have value
                    ]);
                })
            ]);
    };

    func takeTests() : Group {
        return describe("Tree Take Tests", [
                it("should get a range in assending order", do {
                    var tree = getFilledTree(0, 100);
                    let range = RBTree.take<Nat, Nat>(buildFilter(25, 50), #asc, tree).result;
                    
                    var c = 24;
                    let result = List.map<(Nat, Nat), Bool>(range, func(x) {
                        c +=1;
                        c == x.0;
                    });

                    assertAllTrue(List.toArray(result));
                }),
                it("should get a range in decending order", do {
                    var tree = getFilledTree(0, 100);
                    let range = RBTree.take<Nat, Nat>(buildFilter(25, 50), #dsc, tree).result;
                    
                    var c = 51;
                    let result = List.map<(Nat, Nat), Bool>(range, func(x) {
                        c -=1;
                        c == x.0;
                    });

                    assertAllTrue(List.toArray(result));
                })
            ]);
    };

    func testMinMax() : Group {
        return describe("Min Max Tests", [
                it("should get the min value", do {
                    let MIN = 44;

                    var tree = getFilledTree(44, 123);
                    let minMaybe = RBTree.treeMin(tree);

                    assertAllTrue([
                        Optional.unwrap(minMaybe) == MIN, // Original tree does not have value
                    ]);
                }),
                it("should get the max value", do {
                    let MAX = 133;

                    var tree = getFilledTree(33, MAX);
                    let minMaybe = RBTree.treeMax(tree);

                    assertAllTrue([
                        Optional.unwrap(minMaybe) == MAX, // Original tree does not have value
                    ]);
                })
            ]);
    };

    public func getTests() : Group {
        return describe("RBTree Test", [
            simpleTests(),
            takeTests(),
            testMinMax(),
        ])
    };
};