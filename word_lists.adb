with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Word_Lists is

	procedure Add_Word (List: in out Word_List_Type;
				Word: in ASU.Unbounded_String) is

		Aux: Word_List_Type;
		Included: Boolean;

	begin
		Included := False;

		if List = null then

			List := new Cell;
			List.Word := Word;
			List.Count := List.Count + 1;	
			Aux := List;
			Included := True;
		else
			Aux := List;

			-- La segunda palabra es igual a la primera

			if ASU.To_String(List.Word) = ASU.To_String(Word) then
		
				List.Count := List.Count + 1;
				Included := True;
			end if;

			--Recorre la lista 

			while Aux.Next /= null and not Included loop
		
				--Ada.Text_IO.Put_Line ("entra en bucle");

				if ASU.To_String(Aux.Word) = ASU.To_String(Word) then

					Aux.Count := Aux.Count + 1;
					Included := True;
				end if;

				Aux := Aux.Next;
			end loop;

			--Recorre la última celda de la lista
	
			if Aux.Next = null and not Included then
			
				if ASU.To_String(Aux.Word) = ASU.To_String(Word) then

					Aux.Count := Aux.Count + 1;
					Included := True;
				end if;
			end if;

			--Si no está en la lista, añade una celda

			if not Included then

				Aux.Next := new Cell;
				Aux := Aux.Next;
				Aux.Word := Word;
				Aux.Count := Aux.Count + 1;
			end if;	
		end if;

	end Add_Word;

	procedure Delete_Word (List: in out Word_List_Type;
			Word: in ASU.Unbounded_String) is

		Aux: Word_List_Type; 
		Previous: Word_List_Type;
		Found: Boolean;

		procedure Free is new Ada.Unchecked_Deallocation (Cell, Word_List_Type);

	begin
		Found := False;
		Aux := List;
		Previous := Aux;

		while Aux /= null loop
			if ASU.To_String(Previous.Word) = ASU.To_String(Word) then
				List := List.Next;
				Free(Aux);
				Found := True;	
			elsif ASU.To_String(Aux.Word) = ASU.To_String(Word) then
				Previous.Next := Aux.Next;
				Free(Aux);
				Found := True;		
			else
				Previous := Aux;
				Aux := Aux.Next; 
			end if;
		end loop;

		if not Found then
			raise Word_List_Error;
		end if;
	end Delete_Word;

	procedure Search_Word (List: in Word_List_Type;
				Word: in ASU.Unbounded_String;
				Count: out Natural) is

		Finder: Word_List_Type;
		Found: Boolean;			
	begin					
		Finder := List;			
		Count := 0;
		Found := False;		
	
		while Finder /= null loop

			if ASU.To_String(Finder.Word) = ASU.To_String(Word) then
				Count := Finder.Count;
				Found := True;	
			end if;

			Finder := Finder.Next;
		end loop;

		if not Found then
			raise Word_List_Error;
		end if;	
	end Search_Word;

	procedure Max_Word (List: in Word_List_Type;
				Word: out ASU.Unbounded_String;
				Count: out Natural) is
	
		Maxim: Word_List_Type;
	begin
		Maxim := List;
		Count := 0;

		while Maxim.next /= null loop
		
			if Maxim.Count > Count then
				Count := Maxim.Count;
				Word := Maxim.Word;
			end if;
			
			Maxim := Maxim.Next;
		end loop;

		if Maxim.Next = null and Maxim.Count > Count then
				Count := Maxim.Count;
				Word := Maxim.Word;
		end if;
	exception
		--Si no hay ninguna palabra
		when Constraint_Error =>
			Ada.Text_IO.Put_Line("No words.");
	end Max_Word;
	
	procedure Print_All (List: in Word_List_Type) is

		Printer : Word_List_Type;
		Printed : Boolean;

	begin

		Printer := List;
		Printed := False;
		Ada.Text_IO.New_Line;

		while Printer /= null loop
			
			Ada.Text_IO.Put("|" & ASU.To_String(Printer.Word) & "| - " );
			Ada.Text_IO.Put_Line(Integer'Image(Printer.Count));
			Printed := True;
			Printer := Printer.Next;
		end loop;
		
		if not Printed then
			Ada.Text_IO.Put_Line("No words.");
		end if;
	end Print_All;
	
end Word_Lists;
