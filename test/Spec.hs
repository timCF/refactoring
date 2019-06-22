import           Reports    (Aggregate (..), Event (..), aggregate)
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "Report spec" $
    describe "Aggregate" $ do
      let event = Event
                  "2011-12-03T10:15:30Z"
                  4285
                  "SLICE_IT"
                  "1bb53ed1-787b-4543-9def-ea18eef7902e"
      it "can aggregate single event" $
        aggregate [event] `shouldMatchList` [ Agg "2011-12-03:10|10-50" 1
                                            , Agg "2011-12-03:10|10-50|SLICE_IT" 1
                                            , Agg "10-50|SLICE_IT" 1
                                            , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 1
                                            , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 1
                                            ]
      it "can aggregate multiple events" $ do
        let aggs = aggregate [event, event]
        length aggs `shouldBe` 5
        aggregate [event, event] `shouldMatchList`
            [ Agg "2011-12-03:10|10-50" 2
            , Agg "2011-12-03:10|10-50|SLICE_IT" 2
            , Agg "10-50|SLICE_IT" 2
            , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 2
            , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 2
            ]

      it "can aggregate multiple events" $ do
        let aggs = aggregate [event, event]
            events = [ Event "2011-12-03T10:15:30Z" 4285 "SLICE_IT" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-03T12:15:30Z" 1142 "PAY_NOW" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-03T14:15:30Z" 185 "PAY_NOW" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     , Event "2011-12-04T10:15:30Z" 82850 "PAY_LATER" "1bb53ed1-787b-4543-9def-ea18eef7902e"
                     ]
        length aggs `shouldBe` 5
        aggregate events `shouldMatchList`
          [ Agg "10-50|PAY_NOW" 1
          , Agg "10-50|SLICE_IT" 1
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|PAY_LATER" 1
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|PAY_NOW" 2
          , Agg "1bb53ed1-787b-4543-9def-ea18eef7902e|SLICE_IT" 1
          , Agg "2011-12-03:10|10-50" 1
          , Agg "2011-12-03:10|10-50|SLICE_IT" 1
          , Agg "2011-12-03:12|10-50" 1
          , Agg "2011-12-03:12|10-50|PAY_NOW" 1
          , Agg "2011-12-03:14|<10" 1
          , Agg "2011-12-03:14|<10|PAY_NOW" 1
          , Agg "2011-12-03|1bb53ed1-787b-4543-9def-ea18eef7902e" 3
          , Agg "2011-12-04:10|>500" 1
          , Agg "2011-12-04:10|>500|PAY_LATER" 1
          , Agg "2011-12-04|1bb53ed1-787b-4543-9def-ea18eef7902e" 1
          , Agg "<10|PAY_NOW" 1
          , Agg ">500|PAY_LATER" 1
          ]
