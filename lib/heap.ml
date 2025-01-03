module Heap = struct
  include Batteries.Heap
  let singleton v = add v empty
  let pop_min heap =
    if size heap = 0 then None
    else
      Some (find_min heap, del_min heap)
end
