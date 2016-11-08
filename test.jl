@require "./deftype" defhash defequals
@require "." exports...

testset("flat") do
  @test flat(map(ones, [1,2,3])) == ones(6)
  @test flat(([1], [2,3])) == [1,2,3]
  @test flat(([1], (2,[3]))) == [1,2,3]
  @test flat([1 3; 2 4]) == [1,2,3,4]
  @test flat(([1 3; 2 4], [5,(6,)])) == [1,2,3,4,5,6]
  @test flat(([1], (2,3))) == [1,2,3]
  @test flat(([1])) == [1]
  @test flat([]) == []
end

testset("get(object, key)") do
  @test get(Dict(1=>2), 1) == 2
  @test get([2], 1) == 2
  @test isa(@catch(get(Dict(), 1)), KeyError)
end

testset("get_in") do
  @test get_in(Dict(1=>2), ()) == Dict(1=>2)
  @test get_in(Dict(1=>2), (1,)) == 2
  @test get_in(Dict(1=>Dict(2=>3)), (1,2)) == 3
  @test get_in(Dict(1=>Dict(2=>[1,2,3])), (1,2,3)) == 3
  @test isa(@catch(get_in(Dict(), (1,))), Exception)
end

testset("mapcat") do
  @test mapcat(ones, []) == []
  @test mapcat(ones, [3]) == ones(3)
  @test mapcat(ones, [2,3]) == ones(5)
end

testset("compose") do
  @test compose(vcat)(1) == [1]
  @test compose(iseven, vcat)(1) == [false]
  @test compose(ones, prod, vcat)(3) == [1]
  @test compose(iseven, Int)(3) == 0
end

testset("stream piping") do
  open(tempname(), "w+") do file
    @test write(file, IOBuffer("abc")) == 3
    @test IOBuffer("def") |> file === file
    seekstart(file)
    @test readstring(file) == "abcdef"
  end
end

testset("getindex(::IO, ::Integer)") do
  @test IOBuffer("abc")[2]|>Char == 'b'
end

testset("getindex(::IO, ::UnitRange)") do
  open("main.jl") do io
    @test readstring(io[1:100]) == readstring("main.jl")[1:100]
  end
  open("main.jl") do io
    @test readstring(io[5:100]) == readstring("main.jl")[5:100]
  end
end

testset("push") do
  @test push([], 1, 2) == [1,2]
  @test push(Dict(), :a=>1) == Dict(:a=>1)
  @test push(1//2, :num=>2) == 2//2
  @test push(Base.ImmutableDict{Symbol,Int64}(), :a=>1) == Base.ImmutableDict(:a=>1)
end

testset("unshift") do
  @test unshift([1,2,3], 0) == [0,1,2,3]
  @test unshift([1,2,3], 0, -1) == [-1,0,1,2,3]
  @test unshift([1,2,3], 0, :a) == [:a,0,1,2,3]
end

testset("assoc") do
  @test assoc(Dict(), :a, 1) == Dict(:a=>1)
  @test assoc(1//2, :num, 2) == 2//2
  @test assoc([1,2,3], 1, 2) == [2,2,3]
  @test assoc(Base.ImmutableDict{Symbol,Int64}(), :a, 1) == Base.ImmutableDict(:a=>1)
  @test assoc(Dict(:a=>[]), :b, Dict()) == Dict(:a=>[],:b=>Dict())
  @test assoc(Base.ImmutableDict(:a=>1), :b, :c) == Dict(:a=>1,:b=>:c)
  @test assoc((1,2), 1, 2) == (2,2)
  @test isa(@catch(assoc((), 1, 2)), BoundsError)
  @test assoc((1,2), 1, :a) == (:a,2)
end

testset("assoc_in") do
  @test assoc_in(Dict(:a=>[:a,:b]),[:a,1]=>:b) == Dict(:a=>[:b,:b])
  @test assoc_in(Dict(:a=>[:a,:b]),[]=>:b) == :b
  @test assoc_in(Dict(:a=>[:a],:b=>2),[:a,1]=>:b, [:b]=>1) == Dict(:a=>[:b],:b=>1)
end

testset("dissoc") do
  @test dissoc(Dict(:a=>1), :a) == Dict()
  @test dissoc(Dict(:a=>1,:b=>2), :a,:b) == Dict()
  @test dissoc(Dict(:a=>1,:b=>2,:c=>3), :a,:b) == Dict(:c=>3)
  @test dissoc([1,2,3], 2) == [1,3]
end

testset("dissoc_in") do
  @test dissoc_in(Dict(:a=>1), []) == Dict(:a=>1)
  @test dissoc_in(Dict(:a=>1), [:a]) == Dict()
  @test dissoc_in(Dict(:a=>Dict(:b=>1)), [:a,:b]) == Dict(:a=>Dict())
  @test dissoc_in(Dict(:a=>1,:b=>2), [:a], [:b]) == Dict()
end

testset("group") do
  @test group(iseven, [1,2,3,4]) == ([2,4],[1,3])
end

testset("method_defined") do
  @test method_defined(map,[Function,String]) == false
end

testset("curry") do
  testset("basic") do
    @curry add(a,b,c) = a + b + c
    @test add(1,2,3) == 6
    @test add(1,2)(3) == 6
    @test add(1)(2,3) == 6
    @test add(1)(2)(3) == 6
  end

  testset("default parameters") do
    @curry add(a,b,c=2,d=1) = a + b + c + d
    @test add(1)(2) == 6
    @test add(1,2) == 6
    @test add(1)(2,2) == 6
    @test add(1,2,2) == 6
  end

  testset("type annotations") do
    @curry add(a::Int,b::Int,c::Int) = a + b + c
    @test add(1,2,3) == 6
    @test add(1,2)(3) == 6
    @test add(1)(2,3) == 6
    @test add(1)(2)(3) == 6
  end
end

testset("transducers") do
  @test map(iseven, push!, [], 1) == [false]
  @test filter(iseven, push!, [], 1) == []
  @test filter(iseven, push!, [], 2) == [2]
  @test mapcat(vcat, push!, [], 1) == [1]
  @test transduce([map(iseven), filter(identity)], push!, [], 2) == [true]
  @test transduce([map(iseven), filter(identity)], push!, [], 1) == []
end

testset("need") do
  @test need(@spawn 1) ≡ 1
  @test isa(@catch(need(@spawn error("boom"))), ErrorException)
  @test need(Nullable(1)) ≡ 1
  @test isa(@catch(need(Nullable())), NullException)
  @test need(Nullable(), 1) ≡ 1
end

type B; a; b; c; end
defhash(B)
@test hash(B(1,2,3)) == hash(B(1,2,3))
defequals(B)
@test B(1,2,3) == B(1,2,3)

# @type
@type A(a::Int,b=Dict(),c::Any=Vector{Int}())
@test A(1).a == 1
@test A(1).b == Dict()
@test A(1).c == Int[]
@test fieldtype(A, :a) == Int
@test fieldtype(A, :b) == Dict
@test fieldtype(A, :c) == Any

@immutable C(a::Int,b=Dict(),c::Any=Vector{Int}())
@test hash(C(1)) == hash(C(1))
@test C(1) == C(1)
@test C(1, Dict(:a=>1)) == C(1,Dict(:a=>1), Int[])

@immutable D{T}(a)
@test D{:a}("a") == D{:a}("a")
