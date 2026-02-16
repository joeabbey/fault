with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;

-- A sample Ada program
package body Math_Utils is

   type Vector is array (Positive range <>) of Float;

   subtype Small_Int is Integer range 0 .. 100;

   procedure Print_Hello is
   begin
      Put_Line("Hello, World!");
   end Print_Hello;

   function Add(A, B : Integer) return Integer is
   begin
      return A + B;
   end Add;

   function Factorial(N : Natural) return Natural is
   begin
      if N <= 1 then
         return 1;
      else
         return N * Factorial(N - 1);
      end if;
   end Factorial;

   task type Worker is
      entry Start;
      entry Stop;
   end Worker;

   task body Worker is
   begin
      accept Start;
      Put_Line("Worker started");
      accept Stop;
   end Worker;

   protected type Counter is
      procedure Increment;
      function Value return Natural;
   private
      Count : Natural := 0;
   end Counter;

end Math_Utils;
