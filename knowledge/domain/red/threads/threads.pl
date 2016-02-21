% Een thread heeft precondities, en definieert setting information, die variabelen kan bevatten.

% :- episode(head(Vars), [pos-precs], [neg-precs], [code]).
   	
thread_schema([
	type(red:'BringTheCake'),
	preconditions([
		condition(true, [
			rule(Agens, owlr:typeOrSubType, red:'LittleGirl')
		])
	]),
	characters([
		character(Agens),
		character(red:grandma),
		character(red:wolf)
	]),
	location(red:reds_house),
	goals([
	])
]).
		