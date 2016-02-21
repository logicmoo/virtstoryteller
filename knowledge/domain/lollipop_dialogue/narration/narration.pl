% ----------------------
% Templates for schemas
% ----------------------
schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#Greet', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),
    Text = ['"Hello, ', TargetDesc, '!" says ', AgensDesc, ' (Greet)'].
    
schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#AskFor', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    Text = ['"Can I have', PatiensDesc, '?" asks', AgensDesc, ' (AskFor)'].    
    
schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#GiveTo', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Here you go, ', TargetDesc, '" says', AgensDesc, ', giving', PatiensDesc, 'to', TargetDesc, ' (GiveTo)'].     
    
schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#Pay', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_instrument(Schema,Instrument),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Instrument, InstrumentDesc),    
    Text = ['"Here you go, ', PatiensDesc, '" says', AgensDesc, ', giving', InstrumentDesc, 'to', PatiensDesc, ' (Pay)'].     

schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#Sell', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Here you go, ', TargetDesc, '" says', AgensDesc, ', giving', PatiensDesc, 'to', TargetDesc, ' (Sell)'].

        
schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#Buy', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),
    schema_instrument(Schema,Instrument),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc), 
    object_template(Instrument, InstrumentDesc), 
    Text = [AgensDesc, 'buys', PatiensDesc, 'from', TargetDesc, 'with', InstrumentDesc, '(Buy)'].  

schema_template('http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#ToddleOffTo', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc), 
    Text = [AgensDesc, 'toddles off to', TargetDesc, '(ToddleOffTo)'].      
    
relation_template((S, 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#at', O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	Desc = [Sd, 'is at', Od], !.	  	
	
relation_template((S, 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#has', O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	Desc = [Sd, 'has', Od], !.	
	
relation_template((S, 'http://www.owl-ontologies.com/StoryWorldSettings/Lollipop.owl#adjacent', O), Desc) :-
	object_template(S, Sd),
	object_template(O, Od),
	Desc = [Sd, 'is adjacent to', Od], !.	
       
    