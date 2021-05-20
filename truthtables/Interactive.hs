import Logic (createTruthTable, prettyPrintTruthTable, serializeLogic)

main :: IO ()
main = do {
    putStrLn "enter your input (nothing to exit):";
    input <- getLine;
--    if input == "" then return ();
    putStrLn . prettyPrintTruthTable . createTruthTable . serializeLogic $ input;
    main
}