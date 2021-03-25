module Canvas.Tool.Responsify.Rules exposing (Match, MatchResult(..), Rule, Transform)

import Canvas.Tool.Responsify.Info exposing (Info)
import Canvas.Tool.Responsify.Types exposing (Drawn)
import Spec.Element.Model exposing (EitherElement)
import Spec.Model exposing (WithSpec)



-- A rule is either applicable or inapplicable in a given layout
-- situation.


type MatchResult
    = Applicable
    | Inapplicable



-- I DID ORIGINALLY HAVE THE FOLLOWING, BUT I'M NOT SURE HOW WELL THIS
-- NUMERIC SCORING APPROACH FOR RANKING RULES WOULD WORK IN PRACTICE.
--
-- A rule is either:
--
--  * Mandatory, i.e. it must be applied if the it fits the layout
--    situation;
--  * Inapplicable to the layout situation;
--  * A candidate for application, with selection from candidates
--    determined by a numeric score.
-- type Applies
--     = MustApply
--     | Inapplicable
--     | CouldApply Float
-- Each rule has a match function that determines whether the rule
-- is applicable to the layout situation.


type alias Match =
    Info -> Drawn -> ( MatchResult, Info )



-- Each rule has a transform, which transforms the element in the spec
-- to apply the layout rule to the layout situation.


type alias Transform m =
    WithSpec m -> Drawn -> Info -> EitherElement



-- Rules are named so that we can describe what actions are taken
-- during responsification.


type alias Rule m =
    { name : String
    , match : Match
    , transform : Transform m
    }
