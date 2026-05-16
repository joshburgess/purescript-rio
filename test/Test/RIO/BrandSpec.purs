module Test.RIO.BrandSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Brand (Brand, mkBrand, reflectBrand, retagBrand, unbrand)

type UserId = Brand "UserId" Int
type OrderId = Brand "OrderId" Int

type Name = Brand "Name" String

spec :: Spec Unit
spec = describe "RIO.Brand" do
  describe "mkBrand + unbrand" do
    it "round-trips the carrier" do
      let
        uid :: UserId
        uid = mkBrand 7
      unbrand uid `shouldEqual` 7

    it "round-trips a String carrier too" do
      let
        n :: Name
        n = mkBrand "alice"
      unbrand n `shouldEqual` "alice"

  describe "Eq / Ord" do
    it "equates two brands wrapping the same carrier" do
      let
        a :: UserId
        a = mkBrand 5

        b :: UserId
        b = mkBrand 5
      a `shouldEqual` b

    it "compares carriers under the brand" do
      let
        a :: UserId
        a = mkBrand 5

        b :: UserId
        b = mkBrand 9
      compare a b `shouldEqual` LT

  describe "Show" do
    it "shows through to the carrier's Show instance" do
      let
        uid :: UserId
        uid = mkBrand 42
      show uid `shouldEqual` "42"

  describe "Semigroup / Monoid via the carrier" do
    it "appends two String brands" do
      let
        a :: Name
        a = mkBrand "hello, "

        b :: Name
        b = mkBrand "world"
      unbrand (a <> b) `shouldEqual` "hello, world"

  describe "reflectBrand" do
    it "returns the brand tag as a String" do
      let
        uid :: UserId
        uid = mkBrand 1
      reflectBrand uid `shouldEqual` "UserId"

    it "reflects the brand without observing the value" do
      let
        uid :: UserId
        uid = mkBrand 999999
      reflectBrand uid `shouldEqual` "UserId"

  describe "retagBrand" do
    it "rewrites the tag without touching the carrier" do
      let
        uid :: UserId
        uid = mkBrand 7

        oid :: OrderId
        oid = retagBrand uid
      unbrand oid `shouldEqual` 7
      reflectBrand oid `shouldEqual` "OrderId"
