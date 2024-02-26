@use "github.com/jkroso/Rutherford.jl/test.jl" @test testset @catch
@use "." exports...

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

testset("interleave") do
  @test interleave([1,2,3], 'a')|>collect == Union{Int,Char}[1,'a',2,'a',3]
  @test interleave([1], 'a')|>collect == [1]
  @test interleave([], 'a')|>collect == []
end

testset("get(object, key)") do
  @test get(Dict(1=>2), 1) == 2
  @test get([2], 1) == 2
  @test isa(@catch(get(Dict(), 1)), KeyError)
  @test get(Base.UUID(1), :value) == 1
  @test get((:a,:b), 1) == :a
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

testset("pop") do
  @test pop((1,2,3)) == (1,2)
end

testset("append") do
  @test append([], 1, 2) == [1,2]
  @test append(Dict(), :a=>1) == Dict(:a=>1)
  @test append(Base.ImmutableDict{Symbol,Int64}(), :a=>1) == Base.ImmutableDict(:a=>1)
end

testset("prepend") do
  @test prepend([1,2,3], 0) == [0,1,2,3]
  @test prepend([1,2,3], 0, -1) == [-1,0,1,2,3]
  @test prepend([1,2,3], 0, :a) == [:a,0,1,2,3]
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
  @test assoc((1,2), 2, :a) == (1,:a)
  @test assoc((a=1,b="b"), :a, 2) == (a=2,b="b")
  @test assoc((a=1,b="b"), :a, 2, :b, "c") == (a=2,b="c")
  @test assoc((a=1,), :a, "b") == (a="b",)
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

testset("ismethod") do
  @test ismethod(map, (Function,String)) == false
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
  @test need(@async 1) == 1
  @test isa(@catch(need(@async error("boom"))), ErrorException)
end

# @mutable
@mutable A(a::Int,b=Dict(),c::Any=Vector{Int}())
@test A(1).a == 1
@test A(1).b == Dict()
@test A(1).c == Int[]
@test fieldtype(A, :a) == Int
@test fieldtype(A, :b) == Dict{Any,Any}
@test fieldtype(A, :c) == Any

@struct B(a::Int,b=Dict(),c::Any=Vector{Int}())
@test hash(B(1)) == hash(B(1))
@test B(1) == B(1)
@test B(1, Dict{Any,Any}(:a=>1)) == B(1,Dict(:a=>1), Int[])

@struct C{T}(a)
@test C{:a}("a") == C{:a}("a")
@test C{:a}("a") != C{:b}("a")

@struct D(a,b::Union{Missing,Int})
@test D("a").b |> ismissing
@test D("a",1) != D("a",2)
@test D("a",1) == D("a",1)
@test D("a",1) != D("a")
@test D("a") == D("a")

@struct E(a,b::Union{Missing,Int},c="c")
@test E(1) == E(1,missing,"c")
@test E(1,missing) == E(1,missing,"c")
@test E(1,2) == E(1,2,"c")

@struct F(a=1//2)
@test fieldtype(F, :a) == Rational{Int}

@struct G{T}(a="")
@test G{:a}() == G{:a}("")

@struct H()
@test H() == H()

@abstract struct I
  a
  b
end
@abstract struct L <: I
  c
end
@test isabstracttype(I)
@mutable J(c) <: I
@test fieldnames(J) == (:c, :a, :b)
@mutable K <: L
@test fieldnames(K) == (:c, :a, :b)

testset("waitany") do
  c = [Condition(), Condition()]
  p=@async waitany(c...)
  sleep(0)
  notify(c[2], 1, error=true)
  sleep(0)
  @test need(p)[1] == 1
  c = [Channel(32), Channel(32)]
  put!(c[1], 1)
  sleep(0)
  @test waitany(c...)[1] == nothing
end

testset("waitall") do
  c = [Channel(32), Channel(32)]
  put!(c[1], 1)
  put!(c[2], 2)
  @test waitall(c...) == (nothing, nothing)
  c = Condition()
  p = @async waitall(c)
  sleep(0.1)
  notify(c, 1)
  sleep(0.1)
  @test need(p) == (1,)
end

@test convert(NamedTuple, A(1)) == (a=1, b=Dict(), c=Int[])
