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
  @test @catch(get(Dict(), 1)).msg == "can't get property: 1"
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

testset("TruncatedIO") do
  io = open("main.jl")
  head = TruncatedIO(io, 100)
  readstring(head) == readstring("main.jl")[1:100]
  close(io)
end

testset("assoc") do
  @test assoc(Dict(), :a=>1) == Dict(:a=>1)
  @test assoc(1//2, :num => 2) == 2//2
end

testset("dissoc") do
  @test dissoc(Dict(:a=>1), :a) == Dict()
  @test dissoc(Dict(:a=>1,:b=>2), :a,:b) == Dict()
  @test dissoc(Dict(:a=>1,:b=>2,:c=>3), :a,:b) == Dict(:c=>3)
end

testset("group") do
  @test group(iseven, [1,2,3,4]) == ([2,4],[1,3])
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
