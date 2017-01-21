
let i : int ref = ref (-100)
in
  begin
    while !i < 3 do
      i := (!i + 1)
    end;
    !i
  end
end