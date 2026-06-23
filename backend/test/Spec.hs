-- | Test-suite entry point
--
--   Collects all spec modules and runs them in a single Hspec suite.
--   Each spec module is responsible for one area of the codebase:
--
--   * 'Domain.ValidationSpec'  — input validation rules (text, dates, enums)
--   * 'Domain.RecurrenceSpec'  — annual and monthly event recurrence arithmetic
--   * 'Domain.FilterSpec'      — memory search/filter predicates and date parsing
--   * 'Domain.AggregationSpec' — memory statistics aggregation
--   * 'Handler.HelpersSpec'    — HTTP-layer utility functions (dedup, name extraction)
--   * 'Handler.PhotosSpec'     — photo filename safety checks
--   * 'Handler.IntegrationSpec'— end-to-end API tests against a real PostgreSQL database
--   * 'TypesSpec'              — JSON round-trip tests for all public DTOs
module Main (main) where

import qualified Domain.AggregationSpec
import qualified Domain.FilterSpec
import qualified Domain.RecurrenceSpec
import qualified Domain.ValidationSpec
import qualified Handler.HelpersSpec
import qualified Handler.IntegrationSpec
import qualified Handler.PhotosSpec
import Test.Hspec
import qualified TypesSpec

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "Domain.Validation" Domain.ValidationSpec.spec
  describe "Domain.Recurrence" Domain.RecurrenceSpec.spec
  describe "Domain.Filter" Domain.FilterSpec.spec
  describe "Domain.Aggregation" Domain.AggregationSpec.spec
  describe "Handler.Helpers" Handler.HelpersSpec.spec
  describe "Handler.Photos" Handler.PhotosSpec.spec
  describe "Handler.Integration" Handler.IntegrationSpec.spec
  describe "Types" TypesSpec.spec
