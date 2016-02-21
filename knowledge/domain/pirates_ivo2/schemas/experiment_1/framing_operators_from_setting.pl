framing_schema([    type(ps:'FO_2'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oShip_1, swc:hasRegion, ps:oDeck_1)  ])])]).

framing_schema([    type(ps:'FO_3'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oShip_1, swc:hasRegion, ps:oHold_1)  ])])]).       
    
    
framing_schema([    type(ps:'FO_8'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_down, swc:fromGeographicArea, ps:oDeck_1)  ])])]).        
    
framing_schema([    type(ps:'FO_9'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_down, swc:toGeographicArea, ps:oHold_1)  ])])]).            
    
framing_schema([    type(ps:'FO_10'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_down, swc:hasDoor, ps:oHatch_1)  ])])]).      
    
    
framing_schema([    type(ps:'FO_12'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_up, swc:fromGeographicArea, ps:oHold_1)  ])])]).        
    
framing_schema([    type(ps:'FO_13'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_up, swc:toGeographicArea, ps:oDeck_1)  ])])]).            
    
framing_schema([    type(ps:'FO_14'),    preconditions([]),
    effects([        condition(true, [      fact(ps:oLadder_up, swc:hasDoor, ps:oHatch_1)  ])])]).                    