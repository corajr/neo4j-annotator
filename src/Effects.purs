module App.Effects where

import DOM (DOM)
import Database.Neo4J (NEO4J)

type AppEffects = (dom :: DOM, neo4j :: NEO4J)
