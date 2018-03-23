program ArrayHandler;

const TAM = 2;

type vectort = array[1..TAM] of integer;

var v : vectort;

procedure readVector(var vec : vectort);
var i : integer;
begin
  for i := 1 to TAm do
    vec[i] := readint();
  end;

begin
  readVector(v);
  WriteLn(v);

  v[1] := 666;
  WriteLn(v);

  end.