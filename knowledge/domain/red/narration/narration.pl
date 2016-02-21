% ----------------------
% Templates for schemas
% ----------------------

schema_template('http://www.owl-ontologies.com/Red.owl#TellAboutPoisonPlan', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),        
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Well,', TargetDesc, '" says', AgensDesc, ', "I have a plan to poison', PatiensDesc, '. You just hold on!"'].     

schema_template('http://www.owl-ontologies.com/Red.owl#BecomeHungry', Text, Schema) :-
    schema_agens(Schema,Agens),
    object_template(Agens, AgensDesc),
    Text = [AgensDesc, 'became quite hungry'].

schema_template('http://www.owl-ontologies.com/Red.owl#BakeCake', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema, Patiens),
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    Text = [AgensDesc, 'bakes', PatiensDesc].
    
schema_template('http://www.owl-ontologies.com/Red.owl#PoisonFood', Text, Schema) :-
    schema_agens(Schema,Agens),
    object_template(Agens, AgensDesc),
    schema_patiens(Schema,Patiens),
    object_template(Patiens, PatiensDesc),    
    Text = ['With a few drops of cyankali, ', AgensDesc, 'poisons', PatiensDesc].    

schema_template('http://www.owl-ontologies.com/Red.owl#Cry', Text, Schema) :-
    schema_agens(Schema,Agens),
    object_template(Agens, AgensDesc),
    Text = [AgensDesc, 'bursts out in tears'].
    
schema_template('http://www.owl-ontologies.com/Red.owl#WhatToDo', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),    
    Text = ['What should I do,', TargetDesc, '?", asks', AgensDesc].    
    
schema_template('http://www.owl-ontologies.com/Red.owl#SkipTo', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),
    Text = [AgensDesc, 'skips to', TargetDesc].
    
schema_template('http://www.owl-ontologies.com/Red.owl#ShuffleTo', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),    
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),
    Text = [AgensDesc, 'shuffles to', TargetDesc].    
    
schema_template('http://www.owl-ontologies.com/Red.owl#Give', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),        
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Here you go, ', TargetDesc, '" says', AgensDesc, ', giving', PatiensDesc, 'to', TargetDesc].     
    
schema_template('http://www.owl-ontologies.com/Red.owl#Greet', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),        
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Hello, ', TargetDesc, '" says', AgensDesc].   
    
schema_template('http://www.owl-ontologies.com/Red.owl#GreetBack', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_target(Schema,Target),        
    object_template(Agens, AgensDesc),
    object_template(Target, TargetDesc),    
    Text = ['"Oh, hey, ', TargetDesc, '" says', AgensDesc].       
    
schema_template('http://www.owl-ontologies.com/Red.owl#TakeFrom', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),        
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),    
    object_template(Target, TargetDesc),    
    Text = ['"Give me', PatiensDesc, '," says', AgensDesc, 'and forcefully takes it away from', TargetDesc]. 
    
schema_template('http://www.owl-ontologies.com/Red.owl#TellSomeoneTookSomething', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    schema_target(Schema,Target),        
    schema_instrument(Schema,Instrument),         
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),    
    object_template(Target, TargetDesc),    
    object_template(Instrument, InstrumentDesc),        
    Text = ['"Oh', TargetDesc, ', says', AgensDesc, ', "', InstrumentDesc, 'stole', PatiensDesc, 'from me!'].       
              
    
schema_template('http://www.owl-ontologies.com/Red.owl#Eat', Text, Schema) :-
    schema_agens(Schema,Agens),
    schema_patiens(Schema,Patiens),    
    object_template(Agens, AgensDesc),
    object_template(Patiens, PatiensDesc),    
    Text = [AgensDesc, 'eats', PatiensDesc].                
    
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
       
    