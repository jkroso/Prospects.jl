@require "." flat get_in mapcat compose

@test flat(map(ones, [1,2,3])) == ones(6)
@test flat({[1], [2,3]}) == [1,2,3]
@test flat({[1], {2,[3]}}) == [1,2,3]
@test flat([1 3; 2 4]) == [1,2,3,4]
@test flat({[1 3; 2 4], {5,[6]}}) == [1,2,3,4,5,6]
@test flat({[1], (2,3)}) == [1,2,3]
@test flat({[1]}) == [1]
@test flat([]) == []

@test get({1=>2}, 1) == 2
@test get([2], 1) == 2
@test_throws Exception get({}, 1)

@test get_in({1=>2}, []) == {1=>2}
@test get_in({1=>2}, [1]) == 2
@test get_in({1=>{2=>3}}, [1,2]) == 3
@test get_in({1=>{2=>[1,2,3]}}, [1,2,3]) == 3
@test_throws Exception get_in({}, [1])

@test mapcat(ones, []) == []
@test mapcat(ones, [3]) == ones(3)
@test mapcat(ones, [2,3]) == ones(5)

@test compose(vcat)(1) == [1]
@test compose(iseven, vcat)(1) == [false]
@test compose(ones, prod, vcat)(3) == [1]

target_buffer = open(tempname(), "w+")
@test write(target_buffer, IOBuffer("abc")) == 3
seekstart(target_buffer)
@test readall(target_buffer) == "abc"

close(target_buffer)
