with Ada.Text_IO;
with Ada.Command_Line;
with Word_Lists;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

procedure words is

	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package ATI renames Ada.Text_IO;
	package WL renames Word_Lists;

	File_Name: ASU.Unbounded_String;
	Fich: Ada.Text_IO.File_Type;
	List: WL.Word_List_Type;
	Word: ASU.Unbounded_String;
	Count: Natural;


	procedure Print_Menu is
	
	--Imprime el menú interactivo

	begin
		ATI.New_Line;
		ATI.Put_Line("Options");
		ATI.Put_Line("1 Add Word");
		ATI.Put_Line("2 Delete Word");
		ATI.Put_Line("3 Search Word");
		ATI.Put_Line("4 Show all words");
		ATI.Put_Line("5 Quit");
		ATI.New_Line;
		ATI.Put("Your option? ");
	end Print_Menu;

	--Divide las líneas en palabras y las almacena en la lista

	procedure Divide_In_Words (Line: in out Asu.Unbounded_String; Delimiter: in string) is

		Word: ASU.Unbounded_String; 
		Eol: Boolean;
		Position: Integer;

	begin
		Eol := False;
		while not Eol loop
			
			Position := ASU.Index(Line, Delimiter);
			
			-- Si la línea no tiene más palabras
			if ASU.Length(Line) = 0 then 
				Eol := True;

			-- Si ya no hay mas delimitadores
			elsif ASU.Index(Line, Delimiter) = 0 then

				Word := ASU.Tail(Line, ASU.Length(Line) - Position);
				WL.Add_Word(List, Word);
				Eol := True;
			else
				Eol := False;
		
				-- Si hay dos blancos seguidos
				if (Position - 1) = 0 then
					-- Me salta el blanco
					ASU.Tail(Line, ASU.Length(Line) - Position);
				else
					Word := ASU.Head (Line, Position - 1);
					ASU.Tail(Line, ASU.Length(Line) - Position);
					WL.Add_Word(List, Word);
				end if;
			end if;
		end loop;
	end Divide_In_Words;

	--Lee el fichero

	procedure Read_Fich is

	Line: ASU.Unbounded_String;

	begin
		while not ATI.End_Of_File(Fich) loop
			Line := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line(Fich));
			Divide_In_Words(Line, " ");
		end loop;
	end Read_Fich;

	procedure Basic_Mode (List: in out WL.Word_List_Type;
				Word: in out ASU.Unbounded_String;
				Count: in out Natural) is
	
	begin
		WL.Max_Word(List, Word, Count);
		ATI.Put("The most frequent word: ");
		ATI.Put("|" & ASU.To_String(Word) & "| -" );
		ATI.Put_Line(Integer'Image(Count));
		ATI.New_Line;
		ATI.New_Line;

	end Basic_Mode;

	procedure Interactive_Mode (List: in out WL.Word_List_Type;
					Word: in out ASU.Unbounded_String;
					Count: in out Natural) is
	
	Option: Integer;

	begin
		-- Muestra el menú y realiza las diferentes funciones hasta que pulsamos el quit
		loop
			begin
				Print_Menu;	              
				Option := Integer'Value(ATI.Get_Line);

					case Option is

					-- Añade una palabra a la lista
					when 1 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						WL.Add_Word(List, Word);
						ATI.Put_Line("Word |" & ASU.To_String(Word)
						& "| added");

					-- Borra una palabra de la lista
					when 2 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						WL.Delete_Word (List, Word);

						ATI.New_Line;
						ATI.Put("|" & ASU.To_String(Word) & "| "
						& "deleted");
						ATI.New_Line;
				
					-- Busca una palabra en la lista
					when 3 =>
						ATI.Put("Word? ");
						Word := ASU.To_Unbounded_String(ATI.Get_Line);
						WL.Search_Word(List, Word, Count);

						ATI.New_Line;
						ATI.Put("|" & ASU.To_String(Word) & "| - " );
						ATI.Put_Line(Integer'Image(Count));

					-- Imprime la lista
					when 4 =>
						WL.Print_All(List);

					-- Imprime la palabra que más aparece y sale del bucle 
					when 5 =>
						WL.Max_Word(List, Word, Count);
						ATI.Put("The most frequent word: ");
						ATI.Put("|" & ASU.To_String(Word) & "| -" );
						ATI.Put_Line(Integer'Image(Count));
						ATI.New_Line;
						ATI.New_Line;
					when others =>
						ATI.Put_Line("Not implemented");

					end case;

			exception
				when Constraint_Error =>
				ATI.Put_Line("Incorrect Option");		
			end;
		
			exit when Option = 5;
		end loop;
	end Interactive_Mode;

begin
		Count := 0;
		if ACL.Argument(1) = "-i" then
		
			File_Name := ASU.To_Unbounded_String(ACL.Argument(2));
			ATI.Open(Fich, ATI.In_File, ASU.To_String(File_Name));
			Read_Fich;
			Interactive_Mode(List, Word, Count);
			Ada.Text_IO.Close(Fich);
		else
			File_Name := ASU.To_Unbounded_String(ACL.Argument(1));
			ATI.Open(Fich, ATI.In_File, ASU.To_String(File_Name));
			Read_Fich;
			Basic_Mode(List, Word, Count);
			Ada.Text_IO.Close(Fich);

		end if;
	
exception
		when ATI.Name_Error =>
			if ACL.Argument(1) = "-i" then
				ATI.Put_Line(ACL.Argument(2) & ": file not found");
				Interactive_Mode(List, Word, Count);
			elsif ACL.Argument_Count = 1 then
				ATI.Put_Line(ACL.Argument(1) & ": file not found");
			else	
				ATI.Put_Line("usage: words [-i] <filename>");
				ATI.New_Line;
			end if;

		when Constraint_Error =>
			ATI.Put_Line("usage: words [-i] <filename>");
			ATI.New_Line;

end words;
