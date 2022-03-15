{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.QuasiSpec where

import Prelude hiding (lines)

import Control.Exception
import Data.List hiding (lines)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Shakespeare.Text (st)

spec :: Spec
spec = describe "Quasi" $ do
    describe "parseLine" $ do
        it "returns nothing when line is just whitespace" $
            parseLine "         " `shouldBe` Nothing

        it "handles normal words" $
            parseLine " foo   bar  baz" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 1
                        , lineTokens =
                            [ Token "foo"
                            , Token "bar"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles quotes" $
            parseLine "  \"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "foo bar"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles quotes mid-token" $
            parseLine "  x=\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "x=foo bar"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles escaped quote mid-token" $
            parseLine "  x=\\\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "x=\\\"foo"
                            , Token "bar\""
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles unnested parantheses" $
            parseLine "  (foo bar)  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "foo bar"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles unnested parantheses mid-token" $
            parseLine "  x=(foo bar)  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "x=foo bar"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "handles nested parantheses" $
            parseLine "  (foo (bar))  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "foo (bar)"
                            , Token "baz"
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "escaping" $
            parseLine "  (foo \\(bar)  y=\"baz\\\"\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ Token "foo (bar"
                            , Token "y=baz\""
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        it "mid-token quote in later token" $
            parseLine "foo bar baz=(bin\")" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 0
                        , lineTokens =
                            [ Token "foo"
                            , Token "bar"
                            , Token "baz=bin\""
                            ]
                        , lineComments =
                            mempty
                        }
                    )

        describe "comments" $ do
            it "recognizes one line" $ do
                parseLine "-- | this is a comment" `shouldBe`
                    Just
                        ( Line
                            { lineIndent = 0
                            , lineTokens = []
                            , lineComments = pure "this is a comment"
                            }
                        )

            it "works if comment is indented" $ do
                parseLine "  -- | comment" `shouldBe`
                    Just
                        ( Line
                            { lineIndent = 2
                            , lineTokens = []
                            , lineComments = pure "comment"
                            }
                        )

    describe "collectLinesWithComments" $ do
        let doCollect =
                fmap collectLinesWithComments . preparse

        it "comment" $ do
            let subject =
                    [st|
-- | this is a bike
                    |]

            doCollect subject `shouldBe` Just []

        it "a" $ do
            let subject =
                    [st|
Bicycle -- | this is a bike
                    |]

            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines = Line {lineIndent = 0, lineTokens = [Token "Bicycle"], lineComments = pure "this is a bike" } :| []
                        , lwcComments = []
                        }
                    ]
                )

        it "a2" $ do
            let subject =
                    [st|
-- | this is a bike
Bicycle
                    |]

            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines = Line {lineIndent = 0, lineTokens = [Token "Bicycle"], lineComments = pure "this is a bike" } :| []
                        , lwcComments = []
                        }
                    ]
                )
        it "b" $ do
            let subject =
                    [st|
Bicycle -- | this is a bike
    brand String -- | the brand of the bike
    ExtraBike
        foo bar  -- | this is a foo bar
        baz
    deriving Eq
-- | This is a Car
                    |]

            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "Bicycle"], lineComments = pure "this is a bike" } :|
                            [ Line {lineIndent = 4, lineTokens = [Token "brand",Token "String"], lineComments = pure "the brand of the bike"}
                            , Line {lineIndent = 4, lineTokens = [Token "ExtraBike"], lineComments = mempty}
                            , Line {lineIndent = 8, lineTokens = [Token "foo",Token "bar"], lineComments = pure "this is a foo bar"}
                            , Line {lineIndent = 8, lineTokens = [Token "baz"], lineComments = mempty}
                            , Line {lineIndent = 4, lineTokens = [Token "deriving",Token "Eq"], lineComments = mempty}
                            ]
                        , lwcComments = []
                        }
                    ]
                )

        it "c" $ do
            let subject =
                    [st|
-- | This is a Car
Car
    -- | the make of the Car
    make String
    -- | the model of the Car
    model String -- | (model)
    UniqueModel model
    deriving Eq Show
                    |]

            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "Car"], lineComments = pure "This is a Car"} :|
                            [ Line {lineIndent = 4, lineTokens = [Token "make",Token "String"], lineComments = pure "the make of the Car"}
                            , Line {lineIndent = 4, lineTokens = [Token "model",Token "String"], lineComments = ["the model of the Car", "(model)"]}
                            , Line {lineIndent = 4, lineTokens = [Token "UniqueModel",Token "model"], lineComments = mempty}
                            , Line {lineIndent = 4, lineTokens = [Token "deriving",Token "Eq",Token "Show"], lineComments = mempty}
                            ]
                        , lwcComments = []
                        }
                    ]
                )

        it "d" $ do
            let subject =
                    [st|
-- | Doc comments work.
-- | Has multiple lines.
CommentModel
    -- | First line name.
    -- | Second line name.
    name String

    deriving Eq Show
    |]
            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "CommentModel"], lineComments = ["Doc comments work.", "Has multiple lines."] } :|
                            [ Line {lineIndent = 4, lineTokens = [Token "name", Token "String"], lineComments = ["First line name.", "Second line name."]}
                            , Line {lineIndent = 4, lineTokens = [Token "deriving",Token "Eq",Token "Show"], lineComments = mempty}
                            ]
                        , lwcComments =
                           []
                        }
                    ]
                )



        it "big test" $ do
            let subject =
                    [st|
Bicycle -- | this is a bike
    brand String -- | the brand of the bike
    ExtraBike
        foo bar  -- | this is a foo bar
        baz
    deriving Eq
-- | This is a Car
Car
    -- | the make of the Car
    make String
    -- | the model of the Car
    model String
    UniqueModel model
    deriving Eq Show
+Vehicle
    bicycle BicycleId -- | the bike reference
    car CarId         -- | the car reference
                    |]

            doCollect subject `shouldBe`
                ( Just
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "Bicycle"], lineComments = pure "this is a bike" } :|
                            [ Line {lineIndent = 4, lineTokens = [Token "brand",Token "String"], lineComments = pure "the brand of the bike"}
                            , Line {lineIndent = 4, lineTokens = [Token "ExtraBike"], lineComments = mempty}
                            , Line {lineIndent = 8, lineTokens = [Token "foo",Token "bar"], lineComments = pure "this is a foo bar"}
                            , Line {lineIndent = 8, lineTokens = [Token "baz"], lineComments = mempty}
                            , Line {lineIndent = 4, lineTokens = [Token "deriving",Token "Eq"], lineComments = mempty}
                            ]
                        , lwcComments = []
                        }
                    , LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "Car"], lineComments = pure "This is a Car" } :|
                            [ Line {lineIndent = 4, lineTokens = [Token "make",Token "String"], lineComments = pure "the make of the Car"}
                            , Line {lineIndent = 4, lineTokens = [Token "model",Token "String"], lineComments = pure "the model of the Car"}
                            , Line {lineIndent = 4, lineTokens = [Token "UniqueModel",Token "model"], lineComments = mempty}
                            , Line {lineIndent = 4, lineTokens = [Token "deriving",Token "Eq",Token "Show"], lineComments = mempty}
                            ]
                        , lwcComments = []
                        }
                    , LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, lineTokens = [Token "+Vehicle"], lineComments = mempty} :|
                            [ Line {lineIndent = 4, lineTokens = [Token "bicycle",Token "BicycleId"], lineComments = pure "the bike reference"}
                            , Line {lineIndent = 4, lineTokens = [Token "car",Token "CarId"], lineComments = pure "the car reference"}
                            ]
                        , lwcComments = []
                        }
                    ]
                )

    describe "parse" $ do
        let subject =
                [st|
Bicycle -- | this is a bike
    brand String -- | the brand of the bike
    ExtraBike
        foo bar  -- | this is a foo bar
        baz
    deriving Eq
-- | This is a Car
Car
    -- | the make of the Car
    make String
    -- | the model of the Car
    model String
    UniqueModel model
    deriving Eq Show
+Vehicle
    bicycle BicycleId -- | the bike reference
    car CarId         -- | the car reference

                    |]
        let [bicycle, car, vehicle] = parse lowerCaseSettings subject

        it "should parse the `entityHaskell` field" $ do
            getUnboundEntityNameHS bicycle `shouldBe` EntityNameHS "Bicycle"
            getUnboundEntityNameHS car `shouldBe` EntityNameHS "Car"
            getUnboundEntityNameHS vehicle `shouldBe` EntityNameHS "Vehicle"

        it "should parse the `entityDB` field" $ do
            entityDB (unboundEntityDef bicycle) `shouldBe` EntityNameDB "bicycle"
            entityDB (unboundEntityDef car) `shouldBe` EntityNameDB "car"
            entityDB (unboundEntityDef vehicle) `shouldBe` EntityNameDB "vehicle"

        it "should parse the `entityAttrs` field" $ do
            entityAttrs (unboundEntityDef bicycle) `shouldBe` []
            entityAttrs (unboundEntityDef car) `shouldBe` []
            entityAttrs (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `unboundEntityFields` field" $ do
            let simplifyField field =
                    (unboundFieldNameHS field, unboundFieldNameDB field, unboundFieldComments field)
            (simplifyField <$> unboundEntityFields bicycle) `shouldBe`
                [ (FieldNameHS "brand", FieldNameDB "brand", Just "the brand of the bike\n")
                ]
            (simplifyField <$> unboundEntityFields car) `shouldBe`
                [ (FieldNameHS "make", FieldNameDB "make", Just "the make of the Car\n")
                , (FieldNameHS "model", FieldNameDB "model", Just "the model of the Car\n")
                ]
            (simplifyField <$> unboundEntityFields vehicle) `shouldBe`
                [ (FieldNameHS "bicycle", FieldNameDB "bicycle", Just "the bike reference\n")
                , (FieldNameHS "car", FieldNameDB "car", Just "the car reference\n")
                ]

        it "should parse the `entityUniques` field" $ do
            let simplifyUnique unique =
                    (uniqueHaskell unique, uniqueFields unique)
            (simplifyUnique <$> entityUniques (unboundEntityDef bicycle)) `shouldBe` []
            (simplifyUnique <$> entityUniques (unboundEntityDef car)) `shouldBe`
                [ (ConstraintNameHS "UniqueModel", [(FieldNameHS "model", FieldNameDB "model")])
                ]
            (simplifyUnique <$> entityUniques (unboundEntityDef vehicle)) `shouldBe` []

        it "should parse the `entityForeigns` field" $ do
            let [user, notification] = parse lowerCaseSettings [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]
            unboundForeignDefs user `shouldBe` []
            map unboundForeignDef (unboundForeignDefs notification) `shouldBe`
                [ ForeignDef
                    { foreignRefTableHaskell = EntityNameHS "User"
                    , foreignRefTableDBName = EntityNameDB "user"
                    , foreignConstraintNameHaskell = ConstraintNameHS "fk_noti_user"
                    , foreignConstraintNameDBName = ConstraintNameDB "notificationfk_noti_user"
                    , foreignFieldCascade = FieldCascade Nothing Nothing
                    , foreignFields =
                        []
                        -- the foreign fields are not set yet in an unbound
                        -- entity def
                    , foreignAttrs = []
                    , foreignNullable = False
                    , foreignToPrimary = False
                    }
                ]

        it "should parse the `entityDerives` field" $ do
            entityDerives (unboundEntityDef bicycle) `shouldBe` ["Eq"]
            entityDerives (unboundEntityDef car) `shouldBe` ["Eq", "Show"]
            entityDerives (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `entityEntities` field" $ do
            entityExtra (unboundEntityDef bicycle) `shouldBe` Map.singleton "ExtraBike" [["foo", "bar"], ["baz"]]
            entityExtra (unboundEntityDef car) `shouldBe` mempty
            entityExtra (unboundEntityDef vehicle) `shouldBe` mempty

        it "should parse the `entitySum` field" $ do
            entitySum (unboundEntityDef bicycle) `shouldBe` False
            entitySum (unboundEntityDef car) `shouldBe` False
            entitySum (unboundEntityDef vehicle) `shouldBe` True

        it "should parse the `entityComments` field" $ do
            entityComments (unboundEntityDef bicycle) `shouldBe` Just "this is a bike\n"
            entityComments (unboundEntityDef car) `shouldBe` Just "This is a Car\n"
            entityComments (unboundEntityDef vehicle) `shouldBe` Nothing

        describe "empty entity with comment" $ do
            -- PR NOTE: this was an issue I found where:
            -- EmptyEntity -- | this is an empty entity
            --   foo Int
            --
            -- parses the comment, whereas
            --
            -- EmptyEntity -- | this is an empty entity
            --
            -- does not, super edge case, but it is corrected :)
            let [emptyEntity] = parse lowerCaseSettings "EmptyEntity -- | this is an empty entity"

            it "should parse the entity" $ do
                entityHaskell (unboundEntityDef emptyEntity) `shouldBe` EntityNameHS "EmptyEntity"
                entityDB (unboundEntityDef emptyEntity) `shouldBe` EntityNameDB "empty_entity"

            it "should parse the comment" $ do
                entityComments (unboundEntityDef emptyEntity) `shouldBe` Just "this is an empty entity\n"

        describe "custom Id column" $ do
            it "parses custom Id column" $ do
                let definitions = [st|
User
    Id   Text
    name Text
    age  Int
|]
                let [user] = parse lowerCaseSettings definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "Text"]
                (unboundFieldNameHS <$> unboundEntityFields user) `shouldBe`
                    [ FieldNameHS "name"
                    , FieldNameHS "age"
                    ]

            it "errors on duplicate custom Id column" $ do
                let definitions = [st|
User
    Id   Text
    Id   Text
    name Text
    age  Int
|]
                let [user] = parse lowerCaseSettings definitions
                    errMsg = [st|expected only one Id declaration per entity|]
                evaluate (unboundEntityDef user) `shouldThrow`
                    errorCall (T.unpack errMsg)

        describe "primary declaration" $ do
            it "parses Primary declaration" $ do
                let definitions = [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let [user] = parse lowerCaseSettings definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "UserId"]
                (unboundFieldNameHS <$> unboundEntityFields user) `shouldBe`
                    [ FieldNameHS "ref"
                    , FieldNameHS "name"
                    , FieldNameHS "age"
                    ]

            it "errors on duplicate custom Primary declaration" $ do
                let definitions = [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
    Primary name
|]
                let [user] = parse lowerCaseSettings definitions
                    errMsg = [st|expected only one Primary declaration per entity|]
                evaluate (unboundEntityDef user) `shouldThrow`
                    errorCall (T.unpack errMsg)

            it "errors on conflicting Primary/Id declarations" $ do
                let definitions = [st|
User
    Id Text
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let [user] = parse lowerCaseSettings definitions
                    errMsg = [st|Specified both an ID field and a Primary field|]
                evaluate (unboundEntityDef user) `shouldThrow`
                    errorCall (T.unpack errMsg)

        describe "foreign keys" $ do
            let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]

            it "should allow you to modify the FK name via provided function" $ do
                let
                    flippedFK (EntityNameHS entName) (ConstraintNameHS conName) =
                        conName <> entName
                    [_user, notification] =
                        parse (setPsToFKName flippedFK lowerCaseSettings) definitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe`
                        ConstraintNameDB "fk_noti_user_notification"

            it "should allow you to enable snake cased foriegn keys via a preset configuration function" $ do
                let
                    [_user, notification] =
                        parse (setPsUseSnakeCaseForiegnKeys lowerCaseSettings) definitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe`
                        ConstraintNameDB "notification_fk_noti_user"

        describe "ticked types" $ do
            it "should be able to parse ticked types" $ do
                let simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let tickedDefinition = [st|
CustomerTransfer
    customerId CustomerId
    moneyAmount (MoneyAmount 'Customer 'Debit)
    currencyCode CurrencyCode
    uuid TransferUuid
|]
                let [customerTransfer] = parse lowerCaseSettings tickedDefinition
                let expectedType =
                        FTTypeCon Nothing "MoneyAmount" `FTApp` FTTypePromoted "Customer" `FTApp` FTTypePromoted "Debit"

                (simplifyField <$> unboundEntityFields customerTransfer) `shouldBe`
                    [ (FieldNameHS "customerId", FTTypeCon Nothing "CustomerId")
                    , (FieldNameHS "moneyAmount", expectedType)
                    , (FieldNameHS "currencyCode", FTTypeCon Nothing "CurrencyCode")
                    , (FieldNameHS "uuid", FTTypeCon Nothing "TransferUuid")
                    ]

    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" `shouldBe` Right (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar" `shouldBe` Right (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar" `shouldBe` Right (
                FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
        it "application multiple" $
            parseFieldType "Foo Bar Baz" `shouldBe` Right (
                (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                `FTApp` FTTypeCon Nothing "Baz"
                )
        it "parens" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)" `shouldBe` Right (
                foo `FTApp` (bar `FTApp` baz))
        it "lists" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz" `shouldBe` Right (
                foo `FTApp` bars `FTApp` baz)
        it "fails on lowercase starts" $ do
            parseFieldType "nothanks" `shouldBe` Left "PSFail ('n',\"othanks\")"

    describe "#1175 empty entity" $ do
        let subject =
                [st|
Foo
    name String
    age Int

EmptyEntity

Bar
    name String

Baz
    a Int
    b String
    c FooId
                    |]

        let preparsed =
                preparse subject
        it "preparse works" $ do
            (length <$> preparsed) `shouldBe` Just 10

        it "parse works" $ do
            let test name'fieldCount parsedList = do
                    case (name'fieldCount, parsedList) of
                        ([], []) ->
                            pure ()
                        ((name, fieldCount) : _, []) ->
                            expectationFailure
                                $ "Expected an entity with name "
                                <> name
                                <> " and " <> show fieldCount <> " fields"
                                <> ", but the list was empty..."

                        ((name, fieldCount) : ys, (x : xs)) -> do
                            let
                                UnboundEntityDef {..} =
                                    x
                            (unEntityNameHS (getUnboundEntityNameHS x), length unboundEntityFields)
                                `shouldBe`
                                    (T.pack name, fieldCount)
                            test ys xs
                        ([], _:_) ->
                            expectationFailure
                                "more entities parsed than expected"

                result =
                    parse lowerCaseSettings subject
            length result `shouldBe` 4

            test
                [ ("Foo", 2)
                , ("EmptyEntity", 0)
                , ("Bar", 1)
                , ("Baz", 3)
                ]
                result


    describe "preparse" $ do
        prop "omits lines that are only whitespace" $ \len -> do
            ws <- vectorOf len arbitraryWhiteSpaceChar
            pure $ preparse (T.pack ws) === Nothing

        it "recognizes entity" $ do
            let expected =
                    Line { lineIndent = 0, lineTokens = pure (Token "Person"), lineComments = mempty } :|
                    [ Line { lineIndent = 2, lineTokens = Token "name" : [Token "String"], lineComments = mempty }
                    , Line { lineIndent = 2, lineTokens = Token "age" : [Token "Int"], lineComments = mempty }
                    ]
            preparse "Person\n  name String\n  age Int" `shouldBe` Just expected

        it "recognizes comments" $ do
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
            let expected =
                    Line { lineIndent = 0, lineTokens = pure (Token "Foo"), lineComments = mempty } :|
                    [ Line { lineIndent = 2, lineTokens = Token "x" : [Token "X"], lineComments = mempty }
                    , Line { lineIndent = 0, lineTokens = [], lineComments = pure "Hello" }
                    , Line { lineIndent = 0, lineTokens = pure (Token "Bar"), lineComments = mempty }
                    , Line { lineIndent = 1, lineTokens = Token "name" : [Token "String"], lineComments = mempty }
                    ]
            preparse text `shouldBe` Just expected

        it "preparse indented" $ do
            let t = T.unlines
                    [ "  Foo"
                    , "    x X"
                    , "  -- | Comment"
                    , "  -- hidden comment"
                    , "  Bar"
                    , "    name String"
                    ]
                expected =
                    Line { lineIndent = 2, lineTokens = pure (Token "Foo"), lineComments = mempty } :|
                    [ Line { lineIndent = 4, lineTokens = Token "x" : [Token "X"], lineComments = mempty }
                    , Line { lineIndent = 2, lineTokens = [], lineComments = pure "Comment" }
                    , Line { lineIndent = 2, lineTokens = pure (Token "Bar"), lineComments = mempty }
                    , Line { lineIndent = 4, lineTokens = Token "name" : [Token "String"], lineComments = mempty }
                    ]
            preparse t `shouldBe` Just expected

        it "preparse extra blocks" $ do
            let t = T.unlines
                    [ "LowerCaseTable"
                    , "  name String"
                    , "  ExtraBlock"
                    , "    foo bar"
                    , "    baz"
                    , "  ExtraBlock2"
                    , "    something"
                    ]
                expected =
                    Line { lineIndent = 0, lineTokens = pure (Token "LowerCaseTable"), lineComments = mempty } :|
                    [ Line { lineIndent = 2, lineTokens = Token "name" : [Token "String"], lineComments = mempty }
                    , Line { lineIndent = 2, lineTokens = pure (Token "ExtraBlock"), lineComments = mempty }
                    , Line { lineIndent = 4, lineTokens = Token "foo" : [Token "bar"], lineComments = mempty }
                    , Line { lineIndent = 4, lineTokens = pure (Token "baz"), lineComments = mempty }
                    , Line { lineIndent = 2, lineTokens = pure (Token "ExtraBlock2"), lineComments = mempty }
                    , Line { lineIndent = 4, lineTokens = pure (Token "something"), lineComments = mempty }
                    ]
            preparse t `shouldBe` Just expected

        it "field comments" $ do
            let text = T.unlines
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
                expected =
                    Line { lineIndent = 0, lineTokens = [], lineComments = pure "Model" } :|
                    [ Line { lineIndent = 0, lineTokens = [Token "Foo"], lineComments = mempty }
                    , Line { lineIndent = 2, lineTokens = [], lineComments = pure "Field" }
                    , Line { lineIndent = 2, lineTokens = (Token <$> ["name", "String"]), lineComments = mempty }
                    ]
            preparse text `shouldBe` Just expected

    describe "parseLines" $ do
        it "parses comments from consecutive entities" $ do
            let text =
                    T.unlines
                        [ "Bar sql=bars"
                        , "  age Int"
                        , "-- | comment"
                        , "Foo"
                        , "  name String"
                        ]
            let [bar, foo] = parse lowerCaseSettings text
            entityComments (unboundEntityDef bar) `shouldBe` Nothing
            entityComments (unboundEntityDef foo) `shouldBe` Just "comment\n"

        it "works with field comments" $ do
            let text =
                    T.unlines
                        [ "-- | Model"
                        , "Foo"
                        , "  -- | Field a"
                        , "  name String"
                        , "  age String -- | Field b"
                        , "  deleted Bool"
                        ]
            let displayFieldWithComments field =
                    (unboundFieldNameDB field, unboundFieldComments field)
            let [foo] = parse lowerCaseSettings text
            entityComments (unboundEntityDef foo) `shouldBe` Just "Model\n"
            (displayFieldWithComments <$> unboundEntityFields foo) `shouldBe`
                [ (FieldNameDB "name", Just "Field a\n")
                , (FieldNameDB "age", Just "Field b\n")
                , (FieldNameDB "deleted", Nothing)
                ]

        let lines =
                T.unlines
                    [ "-- | Comment"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    , "  age  Int"
                    , "  Extra"
                    , "    foo bar"
                    , "    baz"
                    , "  Extra2"
                    , "    something"
                    ]
        let [subject] = parse lowerCaseSettings lines
        it "produces the right name" $ do
            getUnboundEntityNameHS subject `shouldBe` EntityNameHS "Foo"
        describe "unboundEntityFields" $ do
            let fields = unboundEntityFields subject
            it "has the right field names" $ do
                map unboundFieldNameHS fields `shouldMatchList`
                    [ FieldNameHS "name"
                    , FieldNameHS "age"
                    ]
            it "has comments" $ do
                map unboundFieldComments fields `shouldBe`
                    [ Just "Field\n"
                    , Nothing
                    ]
        it "has the comments" $ do
            entityComments (unboundEntityDef subject) `shouldBe`
                Just "Comment\n"
        it "combines extrablocks" $ do
            entityExtra (unboundEntityDef subject) `shouldBe` Map.fromList
                [ ("Extra", [["foo", "bar"], ["baz"]])
                , ("Extra2", [["something"]])
                ]
        describe "works with extra blocks" $ do
            let [_, lowerCaseTable, idTable] =
                    case parse lowerCaseSettings $ T.unlines
                        [ ""
                        , "IdTable"
                        , "    Id Day default=CURRENT_DATE"
                        , "    name Text"
                        , ""
                        , "LowerCaseTable"
                        , "    Id             sql=my_id"
                        , "    fullName Text"
                        , "    ExtraBlock"
                        , "        foo bar"
                        , "        baz"
                        , "        bin"
                        , "    ExtraBlock2"
                        , "        something"
                        , ""
                        , "IdTable"
                        , "    Id Day default=CURRENT_DATE"
                        , "    name Text"
                        , ""
                        ] of
                            [a, b, c] ->
                                [a, b, c] :: [UnboundEntityDef]
                            xs ->
                                error
                                $ "Expected 3 elements in list, got: "
                                <> show (length xs)
                                <> ", list contents: \n\n" <> intercalate "\n" (map show xs)
            describe "idTable" $ do
                let UnboundEntityDef { unboundEntityDef = EntityDef {..}, .. } = idTable
                it "has no extra blocks" $ do
                    entityExtra `shouldBe` mempty
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "IdTable"
                it "has the right fields" $ do
                    map unboundFieldNameHS unboundEntityFields `shouldMatchList`
                        [ FieldNameHS "name"
                        ]
            describe "lowerCaseTable" $ do
                let UnboundEntityDef { unboundEntityDef = EntityDef {..}, ..} = lowerCaseTable
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "LowerCaseTable"
                it "has the right fields" $ do
                    map unboundFieldNameHS unboundEntityFields `shouldMatchList`
                        [ FieldNameHS "fullName"
                        ]
                it "has ExtraBlock" $ do
                    Map.lookup "ExtraBlock" entityExtra
                        `shouldBe` Just
                            [ ["foo", "bar"]
                            , ["baz"]
                            , ["bin"]
                            ]
                it "has ExtraBlock2" $ do
                    Map.lookup "ExtraBlock2" entityExtra
                        `shouldBe` Just
                            [ ["something"]
                            ]

arbitraryWhiteSpaceChar :: Gen Char
arbitraryWhiteSpaceChar =
  oneof $ pure <$> [' ', '\t', '\n', '\r']
