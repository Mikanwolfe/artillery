program SortArray;


procedure printArray(selectedArray:Array of Integer);
var
	i:Integer;
begin
	for i:=low(selectedArray) to high(selectedArray) do
	begin
		WriteLn('[printArray] :',selectedArray[i]);
	end;
end;

procedure sortArray(var selectedArray: Array of Integer);
//passed by reference in order to sort the years.
var
	i,j:Integer;
	tempArray:Array of Integer;
begin
	setLength(tempArray,Length(selectedArray));
	tempArray[Low(tempArray)]:=selectedArray[Low(selectedArray)];
	for j:=low(selectedArray) to high(selectedArray) do
	begin
		for i:=low(selectedArray) to high(selectedArray) do		//Find the highest number
		begin
			if (tempArray[j] < selectedArray[i]) then
			tempArray[j]:=selectedArray[i];
		end;
		for i:=low(selectedArray) to high(selectedArray) do
		begin
			if (tempArray[j] = selectedArray[i]) then
			selectedArray[i]:=0;
		end;
	end;
	for i:=low(selectedArray) to high(selectedArray) do
	begin
		selectedArray[i]:=tempArray[i];
	end;
	
end;

procedure Main();
var
	testArray: Array[0..6] of Integer;
begin
	testArray[0]:=2006;
	testArray[1]:=2012;
	testArray[2]:=2005;
	testArray[3]:=2009;
	testArray[4]:=2018;
	testArray[5]:=2002;
	
	sortArray(testArray);
	printArray(testArray);
end;

begin
	Main();
end.