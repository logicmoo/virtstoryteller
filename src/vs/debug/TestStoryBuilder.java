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
package vs.debug;

import java.io.File;
import java.util.logging.Logger;

import vs.fabula.BasicFabulaBuilder;
import vs.fabula.IFabulaBuilder;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.rdf.model.Property;
import com.hp.hpl.jena.rdf.model.Resource;


public class TestStoryBuilder {
	
	public static void main(String args[]) {
		TestStoryBuilder tsb = new TestStoryBuilder();
		tsb.run();
	}
	
	private Logger logger;
	
	/*public void run() {
		
		String id = "plop";
		
		IFabulaNetwork root = new FabulaNetworkOld();
		//FabulaNode node;
		IKnowledgeManager km = m_PlotAgent.getStoryBuilder().getKnowledgeManager();
		//km.loadKB(Config.ONTOLOGYPATH + "test.owl");
		
		
		// Plop is hungry
		FabulaNodeOld plopNode = new FabulaNodeOld(id, km.toSymbol("Humanoid", Config.SWC));
		root.putNode(plopNode);
		FabulaNodeOld hungryNode = new FabulaNodeOld(id, km.toSymbol("Hunger", Config.FABULA));
		root.putNode(plopNode);
		
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("plop",null), km.toSymbol("Humanoid",Config.SWC)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("hungry",null), km.toSymbol("Cognition",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, hungryNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, hungryNode.getIndividualName(), 1).toString());
		
		// Plop believes there is an apple in the house
		FabulaNodeOld beliefAppleHouseNode = new FabulaNodeOld(id, km.toSymbol("BeliefElement", Config.FABULA));
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("belief_1",null), km.toSymbol("Belief",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, beliefAppleHouseNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, beliefAppleHouseNode.getIndividualName(), 1).toString());

		FabulaNodeOld appleNode = new FabulaNodeOld(id, km.toSymbol("FruitOrVegetable",Config.SWC));
		FabulaNodeOld houseNode = new FabulaNodeOld(id, km.toSymbol("GeographicArea",Config.SWC));
		root.putNode(appleNode);
		root.putNode(houseNode);
		FabulaNodeOld b_appleNode = new FabulaNodeOld(id, km.toSymbol("FruitOrVegetable",Config.SWC));
		b_appleNode.setContext(beliefAppleHouseNode.getIndividualName());
		FabulaNodeOld b_houseNode = new FabulaNodeOld(id, km.toSymbol("GeographicArea",Config.SWC));
		b_houseNode.setContext(beliefAppleHouseNode.getIndividualName());
		root.putNode(b_appleNode);
		root.putNode(b_houseNode);
		root.addLogic(new TripleString(km.toSymbol("isLocated",Config.SWC), b_appleNode.getIndividualName(), b_houseNode.getIndividualName()).toString());
		
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("apple",null), km.toSymbol("FruitOrVegetable",Config.SWC)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("house",null), km.toSymbol("GeographicArea",Config.SWC)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("b_apple",null), km.toSymbol("FruitOrVegetable",Config.SWC)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("b_house",null), km.toSymbol("GeographicArea",Config.SWC)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("apple",null), km.toSymbol("FruitOrVegetable",Config.SWC)).toString());
		root.addLogic(new TripleString(FabulaStructure.haNetwork, b_appleNode.getIndividualName(), appleNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStructure.haNetwork, b_houseNode.getIndividualName(), houseNode.getIndividualName()).toString());
		//root.addLogic(new TripleString(Fabula.hasContext, km.toSymbol("b_apple",null), km.toSymbol("belief_1",null)).toString());
		//root.addLogic(new TripleString(Fabula.hasContext, km.toSymbol("b_house",null), km.toSymbol("belief_1",null)).toString());
		//root.addLogic(new TripleString(km.toSymbol("isLocated",Config.SWC), km.toSymbol("b_apple",null), km.toSymbol("b_house",null)).toString());
		
		// These two elements cause that Plop wants to eat the apple
		FabulaNodeOld goalNode = new FabulaNodeOld(id, km.toSymbol("AttainGoal",Config.FABULA));
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("goal_1",null), km.toSymbol("Goal",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, goalNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, goalNode.getIndividualName(), 2).toString());
		goalNode.addCause(hungryNode.getIndividualName(),FabulaStrucNetwork);
		goalNode.addCause(beliefAppleHouseNode.getIndividualName(), FabulaStrucNetwork);
		root.putNode(goalNode);
		//root.addLogic(new TripleString(Fabula.psi_causes, km.toSymbol("hungry",null), km.toSymbol("goal_1",null)).toString());
		//root.addLogic(new TripleString(Fabula.psi_causes, km.toSymbol("belief_1",null), km.toSymbol("goal_1",null)).toString());
		
		// Contents of the goal
		FabulaNodeOld g_eatAppleNode = new FabulaNodeOld(id, km.toSymbol("Eat",Config.FABULA));
		root.putNode(g_eatAppleNode);
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("g_eatApple",null), km.toSymbol("Eat",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaNetwork, g_eatAppleNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStNetwork, g_eatAppleNode.getIndividualName(), b_appleNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStrucNetwork, g_eatAppleNode.getIndividualName(), goalNode.getIndividualName()).toString());
		//root.addLogic(new TripleString(Fabula.time, goalNode.getIndividualName(), 3).toString());
		
		// goal motivates: take apple
		FabulaNodeOld takeAppleNode = new FabulaNodeOld(id, km.toSymbol("TakeFrom",Config.FABULA));
		FabulaNodeOld eatAppleNode = new FabulaNodeOld(id, km.toSymbol("Eat",Config.FABULA));
		root.putNode(takeAppleNode);
		root.putNode(eatAppleNode);
		takeAppleNode.addCause(goalNode.getIndividualName(), FabulaStruNetwork);
		eatAppleNode.addCause(goalNode.getIndividualName(), FabulaStruNetwork);
		//root.addLogic(new TripleString(Fabula.motivates, km.toSymbol("goal_1",null), km.toSymbol("takeApple",null)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("takeApple",null), km.toSymbol("TakeFrom",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaNetwork, takeAppleNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStNetwork, takeAppleNode.getIndividualName(), b_appleNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, takeAppleNode.getIndividualName(), 3).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, takeAppleNode.getIndividualName(), 3).toString());
		root.addLogic(new TripleString(FabulaStNetwork, takeAppleNode.getIndividualName(), 4).toString());
		//root.addLogic(new TripleString(Fabula.target, km.toSymbol("takeApple",null), km.toSymbol("hand",null)).toString());
		
		
		
		// goal motivates: eat apple
		//root.addLogic(new TripleString(Fabula.motivates, km.toSymbol("goal_1",null), km.toSymbol("eatApple",null)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("eatApple",null), km.toSymbol("Eat",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaNetwork, eatAppleNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStNetwork, eatAppleNode.getIndividualName(), b_appleNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, eatAppleNode.getIndividualName(), 3).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, eatAppleNode.getIndividualName(), 5).toString());
		root.addLogic(new TripleString(FabulaStNetwork, eatAppleNode.getIndividualName(), 8).toString());	
		
		
		// causes perception
		FabulaNodeOld perceptionNode = new FabulaNodeOld(id, km.toSymbol("See",Config.FABULA));
		root.putNode(perceptionNode);
		perceptionNode.addCause(eatAppleNode.getIndividualName(), FabulaStrucNetwork);
		root.addLogic(new TripleString(FabulNetwork, perceptionNode.getIndividualName(), 9).toString());
		//root.addLogic(new TripleString(Fabula.phi_causes, km.toSymbol("eatApple",null), km.toSymbol("perception",null)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("perception",null), km.toSymbol("Perception",Config.FABULA)).toString());
		
		
		// contents of the perception
		FabulaNodeOld p_eatAppleNode = new FabulaNodeOld(id, km.toSymbol("Eat",Config.FABULA));
		root.putNode(p_eatAppleNode);
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("p_eatApple",null), km.toSymbol("Eat",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaNetwork, p_eatAppleNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStNetwork, p_eatAppleNode.getIndividualName(), appleNode.getIndividualName()).toString());
		root.addLogic(new TripleString(km.toSymbol("isSuccessful",Config.FABULA), p_eatAppleNode.getIndividualName(), "true").toString());
		p_eatAppleNode.setContext(perceptionNode.getIndividualName());
		//root.addLogic(new TripleString(Fabula.hasContext, km.toSymbol("p_eatApple",null), km.toSymbol("perception",null)).toString());

		// causes new belief
		FabulaNodeOld beliefActionSuccessNode = new FabulaNodeOld(id, km.toSymbol("BeliefElement",Config.FABULA));
		root.putNode(beliefActionSuccessNode);
		beliefActionSuccessNode.addCause(perceptionNode.getIndividualName(), FabulaStrucNetwork);
		//root.addLogic(new TripleString(Fabula.psi_causes, km.toSymbol("perception",null), km.toSymbol("belief_2",null)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("belief_2",null), km.toSymbol("Belief",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, beliefActionSuccessNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, beliefActionSuccessNode.getIndividualName(), 10).toString());
		
		// contents of belief
		FabulaNodeOld b_eatAppleNode = new FabulaNodeOld(id, km.toSymbol("Eat",Config.FABULA));
		root.putNode(b_eatAppleNode);
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("b_eatApple",null), km.toSymbol("Eat",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaNetwork, b_eatAppleNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStNetwork, b_eatAppleNode.getIndividualName(), b_appleNode.getIndividualName()).toString());
		b_eatAppleNode.setContext(beliefActionSuccessNode.getIndividualName());
		//root.addLogic(new TripleString(Fabula.hasContext, km.toSymbol("b_eatApple",null), km.toSymbol("belief_2",null)).toString());
		root.addLogic(new TripleString(FabulaStructNetwork, b_eatAppleNode.getIndividualName(), p_eatAppleNode.getIndividualName()).toString());
		
		// causes outcome
		FabulaNodeOld outcomeNode = new FabulaNodeOld(id, km.toSymbol("Outcome",Config.FABULA));
		root.putNode(outcomeNode);
		outcomeNode.addCause(beliefActionSuccessNode.getIndividualName(), FabulaStrucNetwork);
		//root.addLogic(new TripleString(Fabula.psi_causes, km.toSymbol("belief_2",null), km.toSymbol("positiveOutcome",null)).toString());
		//root.addLogic(new TripleString(RDF.type, km.toSymbol("positiveOutcome",null), km.toSymbol("Outcome",Config.FABULA)).toString());
		root.addLogic(new TripleString(FabulaStruNetwork, outcomeNode.getIndividualName(), plopNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulaStrNetwork, outcomeNode.getIndividualName(), goalNode.getIndividualName()).toString());
		root.addLogic(new TripleString(FabulNetwork, outcomeNode.getIndividualName(), 11).toString());
		
		/*
		FabulaNode e = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("E", Config.FABULA));
		root.putNode(e);
		
		Symbol s1 = Symbol.newSymbol("lamb");
		Symbol s2 = Symbol.newSymbol("sword");
		FabulaNode e_cont1 = new FabulaNode(s1,Symbol.newSymbol("Animal","swc"));
		FabulaNode e_cont2 = new FabulaNode(s2,Symbol.newSymbol("Weapon","swc"));
		root.addLogic(new TripleString("has",s1,s2).toString());
		e_cont1.setContext(e.getIndividualName());
		e_cont2.setContext(e.getIndividualName());
		root.putNode(e_cont1);
		root.putNode(e_cont2);
		
		// Perception
		FabulaNode p = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("P", Config.FABULA));
		p.addCause(e.getIndividualName(), Fabula.phi_causes);
		root.putNode(p);
		
		// IE: lamb gone!
		FabulaNode ie1 = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("IE_belief", Config.FABULA));
		ie1.addCause(p.getIndividualName(), Fabula.psi_causes);
		root.putNode(ie1);
		
		// IE: panic!
		FabulaNode ie2 = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("IE_panic", Config.FABULA));
		ie2.addCause(ie1.getIndividualName(), Fabula.psi_causes);
		root.putNode(ie2);

		
		// G: get lamb back
		FabulaNode g = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("G", Config.FABULA));
		g.addCause(ie1.getIndividualName(), Fabula.psi_causes);
		g.addCause(ie1.getIndividualName(), Fabula.psi_causes);
		root.putNode(g);
				
		
		// A: Go to forest
		FabulaNode a = new FabulaNode(m_PlotAgent.getKnowledgeManager().toSymbol("A", Config.FABULA));
		a.addCause(g.getIndividualName(), Fabula.motivates);
		root.putNode(a);
		
		
		String logic = root.getLogic();
		//logger.info(logic);
		m_PlotAgent.getStoryBuilder().getKnowledgeManager().tell(logic);
		
	}*/
	
	/*public void run() {
		IJTPKnowledgeManager km = m_PlotAgent.getStoryBuilder().getKnowledgeManager();
		String id = "plop";
		
		KifLogicBuilder b = new KifLogicBuilder();
		
		FabulaNetwork root = new FabulaNetwork(b);
		
		// Plop is hungry
		FabulaIndividual plop = new FabulaIndividual(id, km.toSymbol("Humanoid", Config.SWC));
		FabulaIndividual hungry = new FabulaIndividual(id, km.toSymbol("Hunger", Config.FABULA));
	
		FabulaTriple plopIsHungry = new FabulaTriple(Fabula.character, hungry.getName(), plop.getName());
		
		root.addIndividual(plop);
		root.addIndividual(hungry);
	
		root.addTriple(plopIsHungry);
		
		
		// He believes there is an apple in the house
		
		FabulaIndividual bel_appleHouse = new FabulaIndividual(id, km.toSymbol("BeliefElement", Config.FABULA));
		root.addIndividual(bel_appleHouse);
		
		FabulaNetwork belief = new FabulaNetwork(b, bel_appleHouse.getName());
		root.addNetwork(belief);
		FabulaIndividual apple = new FabulaIndividual(id, km.toSymbol("Apple", Config.SWC));
		FabulaIndividual house = new FabulaIndividual(id, km.toSymbol("House", Config.SWC));
		FabulaTriple located = new FabulaTriple(km.toSymbol("isLocated",Config.SWC), apple.getName(), house.getName());
		belief.addIndividual(apple);
		belief.addIndividual(house);
		belief.addTriple(located);
		
		// The hunger and the belief cause a goal
		FabulaIndividual eatAppleGoal = new FabulaIndividual(id, km.toSymbol("AttainGoal", Config.SWC));
		root.addIndividual(eatAppleGoal);
		root.addTriple(new FabulaTriple(Fabula.character, eatAppleGoal.getName(), plop.getName()));
		FabulaTriple hungerCausesGoal = new FabulaTriple(Fabula.psi_causes, hungry.getName(), eatAppleGoal.getName());
		FabulaTriple beliefCausesGoal = new FabulaTriple(Fabula.psi_causes, bel_appleHouse.getName(), eatAppleGoal.getName());
		root.addTriple(hungerCausesGoal);
		root.addTriple(beliefCausesGoal);
		
		
		String logic = root.build();
		

		
		m_PlotAgent.getStoryBuilder().getKnowledgeManager().tell(logic);
	}*/
	
	public TestStoryBuilder() {
		logger = LogFactory.getLogger(this);
		
	}
	
	public void run() {
		Model contents = ModelFactory.createDefaultModel();
		Resource adam = contents.createResource("http://test/test.rdf#adam");
		Resource beth = contents.createResource("http://test/test.rdf#beth");
		Property childOf = contents.createProperty("http://test/test.rdf#childOf");
		contents.add(adam, childOf, beth);
		
		IFabulaBuilder sb = new BasicFabulaBuilder(null);
		//sb.addCausality("fabula:goal345", "fabula:motivates", "fabula:action11");
		//sb.addContents("fabula:goal345", contents);
		
		sb.saveFabula(new File("test.trig"), "TRIG");
	}

}