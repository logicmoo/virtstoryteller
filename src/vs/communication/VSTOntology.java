/* Copyright (C) 2008 Human Media Interaction - University of Twente
 * 
 * This file is part of The Virtual Storyteller.
 * 
 * The Virtual Storyteller is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The Virtual Storyteller is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with The Virtual Storyteller. If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package vs.communication;

import jade.content.onto.*;
import jade.content.schema.*;
import jade.util.leap.HashMap;
import jade.content.lang.Codec;
import jade.core.CaseInsensitiveString;

/** file: VSTOntology.java
 * @author ontology bean generator
 * @version 2008/10/29, 15:01:49
 */
public class VSTOntology extends jade.content.onto.Ontology  {
  //NAME
  public static final String ONTOLOGY_NAME = "VST";
  // The singleton instance of this ontology
  private static ReflectiveIntrospector introspect = new ReflectiveIntrospector();
  private static Ontology theInstance = new VSTOntology();
  public static Ontology getInstance() {
     return theInstance;
  }


   // VOCABULARY
    public static final String INCOMINGPERCEPTION_PERCEPTION="perception";
    public static final String INCOMINGPERCEPTION="IncomingPerception";
    public static final String GOALPROGRESS_CHARACTER="character";
    public static final String GOALPROGRESS_GOALSTATUS="goalstatus";
    public static final String GOALPROGRESS_GOAL="goal";
    public static final String GOALPROGRESS="GoalProgress";
    public static final String INCOMINGSETTING_SETTING="setting";
    public static final String INCOMINGSETTING="IncomingSetting";
    public static final String OPERATORRESULT_STATUS="status";
    public static final String OPERATORRESULT_OPERATOR="operator";
    public static final String OPERATORRESULT="OperatorResult";
    public static final String WORLDCHANGE_CONTENTTRIPLE="contentTriple";
    public static final String WORLDCHANGE_TRUTH="truth";
    public static final String WORLDCHANGE_CAUSE="cause";
    public static final String WORLDCHANGE="WorldChange";
    public static final String FABULACAUSALITYDECLARATION_FABULACAUSALITY="fabulaCausality";
    public static final String FABULACAUSALITYDECLARATION="FabulaCausalityDeclaration";
    public static final String FABULAELEMENTDECLARATION_FABULAELEMENT="fabulaElement";
    public static final String FABULAELEMENTDECLARATION="FabulaElementDeclaration";
    public static final String NEXTROUND_ROUNDNUMBER="roundNumber";
    public static final String NEXTROUND="NextRound";
    public static final String GIVECONTROL="GiveControl";
    public static final String PERFORMOPERATOR_OPERATOR="operator";
    public static final String PERFORMOPERATOR="PerformOperator";
    public static final String PLAYCHARACTER_CHARACTERINFO="characterInfo";
    public static final String PLAYCHARACTER="PlayCharacter";
    public static final String USESUGGESTION_SUGGESTION="suggestion";
    public static final String USESUGGESTION="UseSuggestion";
    public static final String FRAMINGOPERATORPOSSIBLE_OPERATOR="operator";
    public static final String FRAMINGOPERATORPOSSIBLE="FramingOperatorPossible";
    public static final String SELECTACTION="SelectAction";
    public static final String TRACE_TRACEINFORMATION="traceInformation";
    public static final String TRACE="Trace";
    public static final String STORYPERCEPTION="StoryPerception";
    public static final String OPERATORSTATUS="OperatorStatus";
    public static final String CHARACTERINFO_INDIVIDUAL="individual";
    public static final String CHARACTERINFO="CharacterInfo";
    public static final String OPERATOR_PATIENS="patiens";
    public static final String OPERATOR_ENDTIME="endtime";
    public static final String OPERATOR_PROLOGDESCRIPTION="prologDescription";
    public static final String OPERATOR_INSTRUMENT="instrument";
    public static final String OPERATOR_TARGET="target";
    public static final String OPERATOR_STARTTIME="starttime";
    public static final String OPERATOR_AGENS="agens";
    public static final String OPERATOR_ISSUCCESSFUL="isSuccessful";
    public static final String OPERATOR="Operator";
    public static final String INFERENCEOPERATOR="InferenceOperator";
    public static final String TRACEINFORMATION_MESSAGE="message";
    public static final String TRACEINFORMATION_VERBOSITY="verbosity";
    public static final String TRACEINFORMATION_TRACEDEPTH="traceDepth";
    public static final String TRACEINFORMATION="TraceInformation";
    public static final String SCHEDULED="Scheduled";
    public static final String ABORTED="Aborted";
    public static final String FINISHED="Finished";
    public static final String GOALSCHEMA_PATIENS="patiens";
    public static final String GOALSCHEMA_INSTRUMENT="instrument";
    public static final String GOALSCHEMA_TARGET="target";
    public static final String GOALSCHEMA_AGENS="agens";
    public static final String GOALSCHEMA_OPPONENT="opponent";
    public static final String GOALSCHEMA="GoalSchema";
    public static final String SCHEMA_PROLOGDESCRIPTION="prologDescription";
    public static final String SCHEMA="Schema";
    public static final String STORYOUTCOME_RESOLVES="resolves";
    public static final String STORYOUTCOME="StoryOutcome";
    public static final String STORYBELIEF="StoryBelief";
    public static final String STORYINTERNALELEMENT="StoryInternalElement";
    public static final String FABULACAUSALITY_OBJECTINDIVIDUAL="objectIndividual";
    public static final String FABULACAUSALITY_SUBJECTINDIVIDUAL="subjectIndividual";
    public static final String FABULACAUSALITY_CAUSALPROPERTY="causalProperty";
    public static final String FABULACAUSALITY="FabulaCausality";
    public static final String STORYSETTINGELEMENT="StorySettingElement";
    public static final String STORYACTION="StoryAction";
    public static final String FABULAELEMENT_TIME="time";
    public static final String FABULAELEMENT_CHARACTER="character";
    public static final String FABULAELEMENT_TYPE="type";
    public static final String FABULAELEMENT_INDIVIDUAL="individual";
    public static final String FABULAELEMENT="FabulaElement";
    public static final String RDFTRIPLE_SUBJECT="subject";
    public static final String RDFTRIPLE_PREDICATE="predicate";
    public static final String RDFTRIPLE_TRUTH="truth";
    public static final String RDFTRIPLE_OBJECT="object";
    public static final String RDFTRIPLE="RDFtriple";
    public static final String STATE_CONTENTFABULA="contentFabula";
    public static final String STATE_CONTENTTRIPLE="contentTriple";
    public static final String STATE="State";
    public static final String STORYGOAL="StoryGoal";
    public static final String STORYEVENT="StoryEvent";
    public static final String FRAMINGOPERATOR="FramingOperator";

  /**
   * Constructor
  */
  private VSTOntology(){ 
    super(ONTOLOGY_NAME, BasicOntology.getInstance());
    try { 

    // adding Concept(s)
    ConceptSchema framingOperatorSchema = new ConceptSchema(FRAMINGOPERATOR);
    add(framingOperatorSchema, vs.communication.FramingOperator.class);
    ConceptSchema storyEventSchema = new ConceptSchema(STORYEVENT);
    add(storyEventSchema, vs.communication.StoryEvent.class);
    ConceptSchema storyGoalSchema = new ConceptSchema(STORYGOAL);
    add(storyGoalSchema, vs.communication.StoryGoal.class);
    ConceptSchema stateSchema = new ConceptSchema(STATE);
    add(stateSchema, vs.communication.State.class);
    ConceptSchema rdFtripleSchema = new ConceptSchema(RDFTRIPLE);
    add(rdFtripleSchema, vs.communication.RDFtriple.class);
    ConceptSchema fabulaElementSchema = new ConceptSchema(FABULAELEMENT);
    add(fabulaElementSchema, vs.communication.FabulaElement.class);
    ConceptSchema storyActionSchema = new ConceptSchema(STORYACTION);
    add(storyActionSchema, vs.communication.StoryAction.class);
    ConceptSchema storySettingElementSchema = new ConceptSchema(STORYSETTINGELEMENT);
    add(storySettingElementSchema, vs.communication.StorySettingElement.class);
    ConceptSchema fabulaCausalitySchema = new ConceptSchema(FABULACAUSALITY);
    add(fabulaCausalitySchema, vs.communication.FabulaCausality.class);
    ConceptSchema storyInternalElementSchema = new ConceptSchema(STORYINTERNALELEMENT);
    add(storyInternalElementSchema, vs.communication.StoryInternalElement.class);
    ConceptSchema storyBeliefSchema = new ConceptSchema(STORYBELIEF);
    add(storyBeliefSchema, vs.communication.StoryBelief.class);
    ConceptSchema storyOutcomeSchema = new ConceptSchema(STORYOUTCOME);
    add(storyOutcomeSchema, vs.communication.StoryOutcome.class);
    ConceptSchema schemaSchema = new ConceptSchema(SCHEMA);
    add(schemaSchema, vs.communication.Schema.class);
    ConceptSchema goalSchemaSchema = new ConceptSchema(GOALSCHEMA);
    add(goalSchemaSchema, vs.communication.GoalSchema.class);
    ConceptSchema finishedSchema = new ConceptSchema(FINISHED);
    add(finishedSchema, vs.communication.Finished.class);
    ConceptSchema abortedSchema = new ConceptSchema(ABORTED);
    add(abortedSchema, vs.communication.Aborted.class);
    ConceptSchema scheduledSchema = new ConceptSchema(SCHEDULED);
    add(scheduledSchema, vs.communication.Scheduled.class);
    ConceptSchema traceInformationSchema = new ConceptSchema(TRACEINFORMATION);
    add(traceInformationSchema, vs.communication.TraceInformation.class);
    ConceptSchema inferenceOperatorSchema = new ConceptSchema(INFERENCEOPERATOR);
    add(inferenceOperatorSchema, vs.communication.InferenceOperator.class);
    ConceptSchema operatorSchema = new ConceptSchema(OPERATOR);
    add(operatorSchema, vs.communication.Operator.class);
    ConceptSchema characterInfoSchema = new ConceptSchema(CHARACTERINFO);
    add(characterInfoSchema, vs.communication.CharacterInfo.class);
    ConceptSchema operatorStatusSchema = new ConceptSchema(OPERATORSTATUS);
    add(operatorStatusSchema, vs.communication.OperatorStatus.class);
    ConceptSchema storyPerceptionSchema = new ConceptSchema(STORYPERCEPTION);
    add(storyPerceptionSchema, vs.communication.StoryPerception.class);

    // adding AgentAction(s)
    AgentActionSchema traceSchema = new AgentActionSchema(TRACE);
    add(traceSchema, vs.communication.Trace.class);
    AgentActionSchema selectActionSchema = new AgentActionSchema(SELECTACTION);
    add(selectActionSchema, vs.communication.SelectAction.class);
    AgentActionSchema framingOperatorPossibleSchema = new AgentActionSchema(FRAMINGOPERATORPOSSIBLE);
    add(framingOperatorPossibleSchema, vs.communication.FramingOperatorPossible.class);
    AgentActionSchema useSuggestionSchema = new AgentActionSchema(USESUGGESTION);
    add(useSuggestionSchema, vs.communication.UseSuggestion.class);
    AgentActionSchema playCharacterSchema = new AgentActionSchema(PLAYCHARACTER);
    add(playCharacterSchema, vs.communication.PlayCharacter.class);
    AgentActionSchema performOperatorSchema = new AgentActionSchema(PERFORMOPERATOR);
    add(performOperatorSchema, vs.communication.PerformOperator.class);
    AgentActionSchema giveControlSchema = new AgentActionSchema(GIVECONTROL);
    add(giveControlSchema, vs.communication.GiveControl.class);

    // adding AID(s)

    // adding Predicate(s)
    PredicateSchema nextRoundSchema = new PredicateSchema(NEXTROUND);
    add(nextRoundSchema, vs.communication.NextRound.class);
    PredicateSchema fabulaElementDeclarationSchema = new PredicateSchema(FABULAELEMENTDECLARATION);
    add(fabulaElementDeclarationSchema, vs.communication.FabulaElementDeclaration.class);
    PredicateSchema fabulaCausalityDeclarationSchema = new PredicateSchema(FABULACAUSALITYDECLARATION);
    add(fabulaCausalityDeclarationSchema, vs.communication.FabulaCausalityDeclaration.class);
    PredicateSchema worldChangeSchema = new PredicateSchema(WORLDCHANGE);
    add(worldChangeSchema, vs.communication.WorldChange.class);
    PredicateSchema operatorResultSchema = new PredicateSchema(OPERATORRESULT);
    add(operatorResultSchema, vs.communication.OperatorResult.class);
    PredicateSchema incomingSettingSchema = new PredicateSchema(INCOMINGSETTING);
    add(incomingSettingSchema, vs.communication.IncomingSetting.class);
    PredicateSchema goalProgressSchema = new PredicateSchema(GOALPROGRESS);
    add(goalProgressSchema, vs.communication.GoalProgress.class);
    PredicateSchema incomingPerceptionSchema = new PredicateSchema(INCOMINGPERCEPTION);
    add(incomingPerceptionSchema, vs.communication.IncomingPerception.class);


    // adding fields
    stateSchema.add(STATE_CONTENTTRIPLE, rdFtripleSchema, 0, ObjectSchema.UNLIMITED);
    stateSchema.add(STATE_CONTENTFABULA, new ConceptSchema("Concept"), 0, ObjectSchema.UNLIMITED);
    rdFtripleSchema.add(RDFTRIPLE_OBJECT, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    rdFtripleSchema.add(RDFTRIPLE_TRUTH, (TermSchema)getSchema(BasicOntology.BOOLEAN), ObjectSchema.OPTIONAL);
    rdFtripleSchema.add(RDFTRIPLE_PREDICATE, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    rdFtripleSchema.add(RDFTRIPLE_SUBJECT, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    fabulaElementSchema.add(FABULAELEMENT_INDIVIDUAL, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    fabulaElementSchema.add(FABULAELEMENT_TYPE, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    fabulaElementSchema.add(FABULAELEMENT_CHARACTER, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    fabulaElementSchema.add(FABULAELEMENT_TIME, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.OPTIONAL);
    fabulaCausalitySchema.add(FABULACAUSALITY_CAUSALPROPERTY, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    fabulaCausalitySchema.add(FABULACAUSALITY_SUBJECTINDIVIDUAL, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    fabulaCausalitySchema.add(FABULACAUSALITY_OBJECTINDIVIDUAL, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    storyOutcomeSchema.add(STORYOUTCOME_RESOLVES, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    schemaSchema.add(SCHEMA_PROLOGDESCRIPTION, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    goalSchemaSchema.add(GOALSCHEMA_OPPONENT, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    goalSchemaSchema.add(GOALSCHEMA_AGENS, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    goalSchemaSchema.add(GOALSCHEMA_TARGET, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    goalSchemaSchema.add(GOALSCHEMA_INSTRUMENT, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    goalSchemaSchema.add(GOALSCHEMA_PATIENS, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    traceInformationSchema.add(TRACEINFORMATION_TRACEDEPTH, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.OPTIONAL);
    traceInformationSchema.add(TRACEINFORMATION_VERBOSITY, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.OPTIONAL);
    traceInformationSchema.add(TRACEINFORMATION_MESSAGE, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_ISSUCCESSFUL, (TermSchema)getSchema(BasicOntology.BOOLEAN), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_AGENS, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_STARTTIME, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_TARGET, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_INSTRUMENT, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_PROLOGDESCRIPTION, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_ENDTIME, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.OPTIONAL);
    operatorSchema.add(OPERATOR_PATIENS, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    characterInfoSchema.add(CHARACTERINFO_INDIVIDUAL, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    traceSchema.add(TRACE_TRACEINFORMATION, traceInformationSchema, ObjectSchema.MANDATORY);
    framingOperatorPossibleSchema.add(FRAMINGOPERATORPOSSIBLE_OPERATOR, operatorSchema, ObjectSchema.MANDATORY);
    useSuggestionSchema.add(USESUGGESTION_SUGGESTION, fabulaElementSchema, ObjectSchema.MANDATORY);
    playCharacterSchema.add(PLAYCHARACTER_CHARACTERINFO, characterInfoSchema, ObjectSchema.MANDATORY);
    performOperatorSchema.add(PERFORMOPERATOR_OPERATOR, operatorSchema, ObjectSchema.MANDATORY);
    nextRoundSchema.add(NEXTROUND_ROUNDNUMBER, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.MANDATORY);
    fabulaElementDeclarationSchema.add(FABULAELEMENTDECLARATION_FABULAELEMENT, fabulaElementSchema, ObjectSchema.OPTIONAL);
    fabulaCausalityDeclarationSchema.add(FABULACAUSALITYDECLARATION_FABULACAUSALITY, fabulaCausalitySchema, ObjectSchema.OPTIONAL);
    worldChangeSchema.add(WORLDCHANGE_CAUSE, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.OPTIONAL);
    worldChangeSchema.add(WORLDCHANGE_TRUTH, (TermSchema)getSchema(BasicOntology.BOOLEAN), ObjectSchema.OPTIONAL);
    worldChangeSchema.add(WORLDCHANGE_CONTENTTRIPLE, rdFtripleSchema, 0, ObjectSchema.UNLIMITED);
    operatorResultSchema.add(OPERATORRESULT_OPERATOR, operatorSchema, ObjectSchema.MANDATORY);
    operatorResultSchema.add(OPERATORRESULT_STATUS, operatorStatusSchema, ObjectSchema.OPTIONAL);
    incomingSettingSchema.add(INCOMINGSETTING_SETTING, storySettingElementSchema, ObjectSchema.MANDATORY);
    goalProgressSchema.add(GOALPROGRESS_GOAL, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    goalProgressSchema.add(GOALPROGRESS_GOALSTATUS, (TermSchema)getSchema(BasicOntology.INTEGER), ObjectSchema.MANDATORY);
    goalProgressSchema.add(GOALPROGRESS_CHARACTER, (TermSchema)getSchema(BasicOntology.STRING), ObjectSchema.MANDATORY);
    incomingPerceptionSchema.add(INCOMINGPERCEPTION_PERCEPTION, storyPerceptionSchema, ObjectSchema.MANDATORY);

    // adding name mappings

    // adding inheritance
    framingOperatorSchema.addSuperSchema(operatorSchema);
    storyEventSchema.addSuperSchema(operatorSchema);
    storyGoalSchema.addSuperSchema(stateSchema);
    stateSchema.addSuperSchema(fabulaElementSchema);
    storyActionSchema.addSuperSchema(operatorSchema);
    storySettingElementSchema.addSuperSchema(stateSchema);
    storyInternalElementSchema.addSuperSchema(stateSchema);
    storyBeliefSchema.addSuperSchema(storyInternalElementSchema);
    storyOutcomeSchema.addSuperSchema(fabulaElementSchema);
    schemaSchema.addSuperSchema(fabulaElementSchema);
    goalSchemaSchema.addSuperSchema(schemaSchema);
    finishedSchema.addSuperSchema(operatorStatusSchema);
    abortedSchema.addSuperSchema(operatorStatusSchema);
    scheduledSchema.addSuperSchema(operatorStatusSchema);
    inferenceOperatorSchema.addSuperSchema(operatorSchema);
    operatorSchema.addSuperSchema(fabulaElementSchema);
    storyPerceptionSchema.addSuperSchema(stateSchema);

   }catch (java.lang.Exception e) {e.printStackTrace();}
  }
  }
