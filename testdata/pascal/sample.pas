unit SampleUnit;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Math;

type
  TCalculator = class
  private
    FLastResult: Double;
  public
    function Add(A, B: Double): Double;
    function Subtract(A, B: Double): Double;
    procedure Reset;
  end;

  TPoint = record
    X, Y: Double;
  end;

procedure PrintHello;
function Factorial(N: Integer): Integer;

implementation

procedure PrintHello;
begin
  WriteLn('Hello, World!');
end;

function Factorial(N: Integer): Integer;
begin
  if N <= 1 then
    Result := 1
  else
    Result := N * Factorial(N - 1);
end;

function TCalculator.Add(A, B: Double): Double;
begin
  FLastResult := A + B;
  Result := FLastResult;
end;

function TCalculator.Subtract(A, B: Double): Double;
begin
  FLastResult := A - B;
  Result := FLastResult;
end;

procedure TCalculator.Reset;
begin
  FLastResult := 0;
end;

end.
