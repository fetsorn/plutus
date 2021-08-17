module Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel where

import           Control.Applicative                                    (liftA2)
import           Control.Monad                                          (liftM4)
import           Data.Maybe                                             (fromJust, fromMaybe, isJust, isNothing)
import           Data.Time                                              (Day)
import           Language.Marlowe.ACTUS.Definitions.ContractTerms       (Cycle (..), IPCB (IPCB_NTL), PPEF (..),
                                                                         PYTP (..), SCEF (..), ScheduleConfig)
import           Language.Marlowe.ACTUS.Definitions.Schedule            (ShiftedDay (..), ShiftedSchedule)
import           Language.Marlowe.ACTUS.Model.Utility.DateShift         (applyBDCWithCfg)
import           Language.Marlowe.ACTUS.Model.Utility.ScheduleGenerator (generateRecurrentScheduleWithCorrections, inf,
                                                                         minusCycle, plusCycle, remove)

-- Principal at Maturity (PAM)

_S :: Day -> Cycle -> Day -> ScheduleConfig -> ShiftedSchedule
_S = generateRecurrentScheduleWithCorrections

_S' :: Maybe Day -> Maybe Cycle -> Maybe Day -> Maybe ScheduleConfig -> Maybe ShiftedSchedule
_S' = liftM4 generateRecurrentScheduleWithCorrections

shift :: ScheduleConfig -> Day -> ShiftedDay
shift = applyBDCWithCfg

_SCHED_IED_PAM :: ScheduleConfig -> Day -> Maybe [ShiftedDay]
_SCHED_IED_PAM scfg _IED = Just [shift scfg _IED]

_SCHED_MD_PAM :: ScheduleConfig -> Day -> Maybe [ShiftedDay]
_SCHED_MD_PAM scfg tmd = Just [shift scfg tmd]

_SCHED_PP_PAM :: ScheduleConfig -> PPEF -> Maybe Cycle -> Day -> Maybe Day -> Day -> Maybe ShiftedSchedule
_SCHED_PP_PAM _    PPEF_N _OPCL _IED _OPANX _MD = Nothing
_SCHED_PP_PAM scfg _PREF  _OPCL _IED _OPANX _MD =
    let maybeS  | isNothing _OPANX && isNothing _OPCL = Nothing
                | isNothing _OPANX                    = Just $ _IED `plusCycle` fromJust _OPCL
                | otherwise                           = _OPANX
    in (\s -> _S s (fromJust _OPCL) _MD scfg) <$> maybeS

_SCHED_PY_PAM :: ScheduleConfig -> PYTP -> PPEF -> Maybe Cycle -> Day -> Maybe Day -> Day -> Maybe ShiftedSchedule
_SCHED_PY_PAM _    PYTP_O _PREF _OPCL _IED _OPANX _MD = Nothing
_SCHED_PY_PAM scfg _PYTP  _PREF _OPCL _IED _OPANX _MD = _SCHED_PP_PAM scfg _PREF _OPCL _IED _OPANX _MD

_SCHED_FP_PAM :: (Eq a, Fractional a) => ScheduleConfig -> a -> Maybe Cycle -> Day -> Maybe Day -> Day -> Maybe ShiftedSchedule
_SCHED_FP_PAM scfg _FER _FECL _IED _FEANX _MD =
    let maybeS  | isNothing _FEANX && isNothing _FECL = Nothing
                | isNothing _FEANX                    = Just $ _IED `plusCycle` fromJust _FECL
                | otherwise                           = _FEANX

        result  | _FER == 0.0                         = Nothing
                | otherwise                           = (\s -> _S s (fromJust _FECL){ includeEndDay = True } _MD scfg) <$> maybeS
    in result

_SCHED_PRD_PAM :: ScheduleConfig -> Maybe Day -> Maybe [ShiftedDay]
_SCHED_PRD_PAM scfg (Just _PRD) = Just [shift scfg _PRD]
_SCHED_PRD_PAM _ _              = Nothing

_SCHED_TD_PAM :: ScheduleConfig -> Maybe Day -> Maybe [ShiftedDay]
_SCHED_TD_PAM scfg (Just _TD) = Just [shift scfg _TD]
_SCHED_TD_PAM _    _          = Nothing

_SCHED_IP_PAM :: ScheduleConfig -> Maybe a -> Day -> Maybe Day -> Maybe Cycle -> Maybe Day -> Day -> Maybe [ShiftedDay]
_SCHED_IP_PAM scfg _IPNR _IED _IPANX _IPCL _IPCED _MD =
    let maybeS  | isNothing _IPANX && isNothing _IPCL = Nothing
                | isNothing _IPANX                    = Just $ _IED `plusCycle` fromJust _IPCL
                | otherwise                           = _IPANX

        result  | isNothing _IPNR                     = Nothing
                | otherwise                           = (\s -> _S s (fromJust _IPCL){ includeEndDay = True } _MD scfg) <$> maybeS

        result' | isJust result && isJust _IPCED      = Just $ filter (\ss -> calculationDay ss > fromJust _IPCED) $ fromJust result
                | otherwise                           = result
    in result'

_SCHED_IPCI_PAM :: ScheduleConfig -> Day -> Maybe Day -> Maybe Cycle -> Maybe Day -> Day -> Maybe a -> Maybe [ShiftedDay]
_SCHED_IPCI_PAM scfg _IED _IPANX _IPCL _IPCED _MD _IPNR =
    -- calculate IP sched:
    let maybeS  | isNothing _IPANX && isNothing _IPCL = Nothing
                | isNothing _IPANX                    = Just $ _IED `plusCycle` fromJust _IPCL
                | otherwise                           = _IPANX

        schedIP | isNothing _IPNR                     = Nothing
                | otherwise                           = (\s -> _S s (fromJust _IPCL){ includeEndDay = True } _MD scfg) <$> maybeS

        result  | isJust _IPCL && isJust _IPCED       = Just $ filter (\s -> calculationDay s < fromJust _IPCED) (fromJust schedIP) ++ [shift scfg $ fromJust _IPCED]
                | otherwise = Nothing
    in result

_SCHED_RR_PAM :: ScheduleConfig -> Day -> Day -> Maybe Day -> Maybe Cycle -> Maybe a -> Day -> Maybe [ShiftedDay]
_SCHED_RR_PAM scfg _IED _SD _RRANX _RRCL _RRNXT _MD =
    let maybeS  | isNothing _RRANX                    = Just $ _IED `plusCycle` fromJust _RRCL
                | otherwise                           = _RRANX

        tt      = (\s -> _S s (fromJust _RRCL){ includeEndDay = False } _MD scfg) <$> maybeS
        trry    = fromJust $ inf (fromJust tt) _SD

        result  | isNothing _RRANX && isNothing _RRCL = Nothing
                | isJust _RRNXT                       = remove trry <$> tt
                | otherwise                           = tt
    in result

_SCHED_RRF_PAM :: ScheduleConfig -> Day -> Maybe Day -> Maybe Cycle -> Day -> Maybe ShiftedSchedule
_SCHED_RRF_PAM scfg _IED _RRANX _RRCL _MD =
    let maybeS  | isNothing _RRANX                    = Just $ _IED `plusCycle` fromJust _RRCL
                | otherwise                           = _RRANX

        tt      = (\s -> _S s (fromJust _RRCL) _MD scfg) <$> maybeS

        result  | isNothing _RRANX && isNothing _RRCL = Nothing
                | otherwise                           = tt
    in result

_SCHED_SC_PAM :: ScheduleConfig -> Day -> SCEF -> Maybe Day -> Maybe Cycle -> Day -> Maybe ShiftedSchedule
_SCHED_SC_PAM scfg _IED _SCEF _SCANX _SCCL _MD =
    let maybeS  | isNothing _SCANX && isNothing _SCCL = Nothing
                | isNothing _SCANX                    = Just $ _IED `plusCycle` fromJust _SCCL
                | otherwise                           = _SCANX

        tt      = (\s -> _S s (fromJust _SCCL){ includeEndDay = False } _MD scfg) <$> maybeS

        result  | _SCEF == SE_000                     = Nothing
                | otherwise                           = tt
    in result

-- Linear Amortizer (LAM)

_SCHED_PR_LAM :: ScheduleConfig -> Maybe Cycle -> Day -> Maybe Day -> Day -> Maybe ShiftedSchedule
_SCHED_PR_LAM scfg _PRCL _IED _PRANX _MD =
    let maybeS  | isNothing _PRANX && isNothing _PRCL = Nothing
                | isNothing _PRANX                    = Just $ _IED `plusCycle` fromJust _PRCL
                | otherwise                           = _PRANX
    in (\s -> _S s (fromJust _PRCL) _MD scfg ) <$> maybeS

_SCHED_MD_LAM :: ScheduleConfig -> Day -> Maybe [ShiftedDay]
_SCHED_MD_LAM scfg tmd = Just [shift scfg tmd]

_SCHED_IPCB_LAM :: ScheduleConfig -> Day -> Maybe IPCB -> Maybe Cycle -> Maybe Day -> Day -> Maybe ShiftedSchedule
_SCHED_IPCB_LAM scfg _IED _IPCB _IPCBCL _IPCBANX _MD =
    let maybeS  | isNothing _IPCBANX && isNothing _IPCBCL = Nothing
                | isNothing _IPCBANX                      = Just $ _IED `plusCycle` fromJust _IPCBCL
                | otherwise                               = _IPCBANX

        result  | fromJust _IPCB /= IPCB_NTL              = Nothing -- This means that IPCB != 'NTL', since there is no cycle
                | otherwise                               = (\s -> _S s (fromJust _IPCBCL) _MD scfg) <$> maybeS
    in result

-- Negative Amortizer (NAM)

_SCHED_IP_NAM :: ScheduleConfig -> Maybe Day -> Maybe Cycle -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe Cycle -> Maybe Day -> Maybe [ShiftedDay]
_SCHED_IP_NAM scfg _IED _PRCL _PRANX _IPCED _IPANX _IPCL _MD =
    let maybeS  | isNothing _PRANX = liftA2 plusCycle _IED _PRCL
                | otherwise        = _PRANX

        _T      = liftA2 minusCycle maybeS _PRCL

        r       | isJust _IPCED = _IPCED
                | isJust _IPANX = _IPANX
                | isJust _IPCL  = liftA2 plusCycle _IED _IPCL
                | otherwise     = Nothing

        u       | isNothing _IPANX && isNothing _IPCL                         = Nothing
                | isJust _IPCED    && fromMaybe False (liftA2 (>=) _IPCED _T) = Nothing
                | otherwise                                                   = _S' r _IPCL _MD (Just scfg)

        v       = _S' maybeS _PRCL _MD (Just scfg)

    in liftA2 (++) u v
