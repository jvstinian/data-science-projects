with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    function Gcd(A, B: Integer) return Integer is
        R: Integer;
    begin
	R := A rem B;
	if R = 0
	then
	    return B;
	else
	    return Gcd(B, R);
	end if;
    end Gcd;

    A : Integer := 221;
    B : Integer := 26;
    R : Integer;
begin
    R := Gcd(A, B);
    Put("GCD" & A'Image & ", " & B'Image & ") = ");
    Put(R'Image);
    New_Line;
end Main;
