#ifndef MACROS_H
#define MACROS_H

#ifndef NO_WOT
-- keep in sync with the number of arguments in
-- Mgw.Model.WithObjType
#define WotUniformTuple(f) (f,f,f,f,f,f,f)
#define WotNumberedTuple(f) (f##01,f##02,f##03,f##04,f##05,f##06,f##07)
#define WotApply(f) (wotApply WotUniformTuple(f))
#define WotApplyM(f) (wotApplyM WotUniformTuple(f))
#define WotMap(f) (wotMap WotUniformTuple(f))
#define WotMapM(f) (wotMapM WotUniformTuple(f))

#define TyWotCase(objType,f) \
  case (objType) of {                                   \
    ObjTypeRoot -> f IsRoot;                            \
    ObjTypeUserList -> f IsUserList;                    \
    ObjTypeUser -> f IsUser;                            \
    ObjTypeIDoc -> f IsIDoc;                            \
    ObjTypeResource -> f IsResource;                    \
    ObjTypeGen -> f IsGen;                              \
    ObjTypeQ -> f IsQ;                                  \
  }

#define WotCase(wot, f)                                          \
  case (wot) of {                                                \
    (IsRoot x) -> f x;                                           \
    (IsUserList x) -> f x;                                       \
    (IsUser x) -> f x;                                           \
    (IsIDoc x) -> f x;                                           \
    (IsResource x) -> f x;                                       \
    (IsGen x) -> f x;                                            \
    (IsQ x) -> f x;                                              \
  }

#define WotCaseTy(wot, f)                                        \
  case (wot) of {                                                \
    (IsRoot x) -> f IsRoot x;                                    \
    (IsUserList x) -> f IsUserList x;                            \
    (IsUser x) -> f IsUser x;                                    \
    (IsIDoc x) -> f IsIDoc x;                                    \
    (IsResource x) -> f IsResource x;                            \
    (IsGen x) -> f IsGen x;                                      \
    (IsQ x) -> f IsQ x;                                          \
  }

#define WotCase2(wotL,wotR,match,nomatch)                               \
  case wotL of {                                                \
    IsRoot valL ->                                                      \
      case wotR of { IsRoot valR -> (match) IsRoot; _ -> (nomatch) };   \
    IsUserList valL ->                                                  \
      case wotR of { IsUserList valR -> (match) IsUserList; _ -> (nomatch) }; \
    IsUser valL ->                                                      \
      case wotR of { IsUser valR -> (match) IsUser; _ -> (nomatch) };       \
    IsIDoc valL -> \
      case wotR of { IsIDoc valR -> (match) IsIDoc; _ -> (nomatch) };       \
    IsResource valL -> \
      case wotR of { IsResource valR -> (match) IsResource; _ -> (nomatch) }; \
    IsGen valL -> \
      case wotR of { IsGen valR -> (match) IsGen; _ -> (nomatch) };  \
    IsQ valL -> \
      case wotR of { IsQ valR -> (match) IsQ; _ -> (nomatch) }; \
  }

#define WotCase2f(wotL,wotR,match,nomatch)                               \
  case wotL of {                                                \
    IsRoot valL ->                                                      \
      case wotR of { IsRoot valR -> ((match) valL valR) IsRoot; _ -> (nomatch) };   \
    IsUserList valL ->                                                  \
      case wotR of { IsUserList valR -> ((match) valL valR) IsUserList; _ -> (nomatch) }; \
    IsUser valL ->                                                      \
      case wotR of { IsUser valR -> ((match) valL valR) IsUser; _ -> (nomatch) };       \
    IsIDoc valL -> \
      case wotR of { IsIDoc valR -> ((match) valL valR) IsIDoc; _ -> (nomatch) };       \
    IsResource valL -> \
      case wotR of { IsResource valR -> ((match) valL valR) IsResource; _ -> (nomatch) }; \
    IsGen valL -> \
      case wotR of { IsGen valR -> ((match) valL valR) IsGen; _ -> (nomatch) }; \
    IsQ valL -> \
      case wotR of { IsQ valR -> ((match) valL valR) IsQ; _ -> (nomatch) }; \
  }

#endif /* NO_WOT */

#define safeUndef (error (__FILE__ ++ ":" ++ show (__LINE__::Int) ++ ": undefined!"))
#define safeRead (Mgw.Util.Misc.readNoteVerbose (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeFromJust (Safe.fromJustNote (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeFromJustNote (\s -> Safe.fromJustNote (__FILE__ ++ ":" ++ show (__LINE__::Int) ++ ": " ++ s))
#define safeError (\safeErr -> error (__FILE__ ++ ":" ++ show (__LINE__::Int) ++ ": ERROR: " ++ safeErr))
#define safeFail (\x -> fail (__FILE__ ++ ":" ++ show (__LINE__::Int) ++ ": FAIL: " ++ x))
#define safeHead (Safe.headNote (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeTail (Safe.tailNote (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeHeadNote (\x -> Safe.headNote (__FILE__ ++ ":" ++ show (__LINE__::Int) ++ ": " ++ x))
#define safeFromRight (Mgw.Util.Misc.fromRightNote (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeFromLeft (Mgw.Util.Misc.fromLeftNote (__FILE__ ++ ":" ++ show (__LINE__::Int)))
#define safeAssertNF (GHC.AssertNF.assertNFNamed ("assertNF failure at " ++ __FILE__ ++ ":" ++ show (__LINE__::Int)))

#define logTrace (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logDebug (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logInfo  (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logNote  (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logWarn  (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logWarnAndFail  (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN Mgw.Util.Logging.noExtraLogInfo (Left m) >> safeFail m)
#define logError (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logAlert (\m -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT Mgw.Util.Logging.noExtraLogInfo (Left m))
#define logAndFail (\s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT Mgw.Util.Logging.noExtraLogInfo (Left s) >> safeFail s)

#define logInfoOnStderr  (\m -> System.IO.hPutStrLn System.IO.stderr m >> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO (Mgw.Util.Logging.noExtraLogInfo { Mgw.Util.Logging.eli_ignoreStderr = True }) (Left m))
#define logNoteOnStderr  (\m -> System.IO.hPutStrLn System.IO.stderr m >> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE (Mgw.Util.Logging.noExtraLogInfo { Mgw.Util.Logging.eli_ignoreStderr = True }) (Left m))
#define logWarnOnStderr  (\m -> System.IO.hPutStrLn System.IO.stderr m >> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN (Mgw.Util.Logging.noExtraLogInfo { Mgw.Util.Logging.eli_ignoreStderr = True }) (Left m))
#define logErrorOnStderr  (\m -> System.IO.hPutStrLn System.IO.stderr m >> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.noExtraLogInfo { Mgw.Util.Logging.eli_ignoreStderr = True }) (Left m))
#define logAlertOnStderr  (\m -> System.IO.hPutStrLn System.IO.stderr m >> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.noExtraLogInfo { Mgw.Util.Logging.eli_ignoreStderr = True }) (Left m))

#define pureTrace (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureDebug (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureInfo (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureNote (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureWarn (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureError (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR Mgw.Util.Logging.noExtraLogInfo (Left m)) x))
#define pureAlert (\m x -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT Mgw.Util.Logging.noExtraLogInfo (Left m)) x))

#define pureTraceWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureDebugWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureInfoWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureNoteWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureWarnWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureErrorWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))
#define pureAlertWithTag (\logTag m logRes -> ((Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkTag logTag) (Left m)) logRes))

#define logTraceWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkTag x) (Left s))
#define logDebugWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkTag x) (Left s))
#define logInfoWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO (Mgw.Util.Logging.mkTag x) (Left s))
#define logNoteWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE (Mgw.Util.Logging.mkTag x) (Left s))
#define logWarnWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN (Mgw.Util.Logging.mkTag x) (Left s))
#define logErrorWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkTag x) (Left s))
#define logAlertWithTag (\x s -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkTag x) (Left s))

#define dumpTrace (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpDebug (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpInfo  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpNote  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpWarn  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpError (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)
#define dumpAlert (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat)


#define appendTrace (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendDebug (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendInfo  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendNote  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendWarn  (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendError (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendAlert (\fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)

#define appendTraceWithTag (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendDebugWithTag (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendInfoWithTag  (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendNoteWithTag  (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendWarnWithTag  (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendErrorWithTag (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)
#define appendAlertWithTag (\t fn dat -> Mgw.Util.Logging.doLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpAppendWithPrefix) dat)


#define pureDumpTrace (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpDebug (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpInfo  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpNote  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpWarn  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpError (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpAlert (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpOverwrite) dat logRes))

#define pureDumpTraceWithTag (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpDebugWithTag (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpInfoWithTag  (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpNoteWithTag  (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpWarnWithTag  (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpErrorWithTag (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))
#define pureDumpAlertWithTag (\t fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkTagDump t fn Mgw.Util.Logging.DumpOverwrite) dat logRes))

#define pureAppendTrace (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.TRACE (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendDebug (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.DEBUG (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendInfo  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.INFO  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendNote  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.NOTE  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendWarn  (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.WARN  (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendError (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ERROR (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))
#define pureAppendAlert (\fn dat logRes -> (Mgw.Util.Logging.unsafeDoLog (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int) Mgw.Util.Logging.ALERT (Mgw.Util.Logging.mkDump fn Mgw.Util.Logging.DumpAppendWithPrefix) dat logRes))

#define runTx (Mgw.Util.Tx.trackTx Nothing (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int))
#define runTxWithName (\name -> Mgw.Util.Tx.trackTx (Just name) (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int))
#define runLazyTx (\name -> Mgw.Util.Tx.trackLazyTx (Just name) Nothing (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int))
#define runLazyTxWithExtraEval (\name eval -> Mgw.Util.Tx.trackLazyTx (Just name) (Just eval) (Mgw.Util.Logging.adjustSourceFilePath __FILE__) (__LINE__::Int))

#define pureLogConv(what,srcNameExp,srcValueExp,srcShowExp,tgtNameExp,convFunExp,tgtShowExp) \
    let __srcValue = srcValueExp in                                \
    let __srcName = srcNameExp in                              \
    let __tgtName = tgtNameExp in                                     \
    let __tgtValue = (convFunExp) __srcValue in                   \
    pureInfoWithTag "conv" ("Converting " ++ what ++ " from " ++ __srcName ++ " to " ++ __tgtName) \
    pureTraceWithTag "conv" (what ++ " " ++ __srcName ++ " input:\n" ++ (srcShowExp) __srcValue) \
    pureDebugWithTag "conv" (what ++ " " ++ __tgtName ++ " output:\n" ++ (tgtShowExp) __tgtValue) \
    __tgtValue

#define pureLogConvShow(what,srcNameExp,srcValueExp,tgtNameExp,convFunExp) \
    pureLogConv(what,srcNameExp,srcValueExp,show,tgtNameExp,convFunExp,show)

#define SrcStr(expr) ("["++(Mgw.Util.Logging.adjustSourceFilePath __FILE__)++":"++show (__LINE__::Int)++":"++expr++"]")

#define DeriveLogMonad(context, head, lift) \
    instance context => LogMonad head where { \
        doLog file line level extraInfo msg = lift (doLog file line level extraInfo msg) }


#endif
