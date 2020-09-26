{-# language GADTs #-}
{-# language KindSignatures #-}

module Web.BIR.BIR11.Types.Report where

import Data.Kind (Type)
import Data.Text (Text)

import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsFizycznej ( Bir11JednLokalnaOsFizycznej )
import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsFizycznejPkd ( Bir11JednLokalnaOsFizycznejPkd )
import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsPrawnej ( Bir11JednLokalnaOsPrawnej )
import Web.BIR.BIR11.Types.Report.Bir11JednLokalnaOsPrawnejPkd ( Bir11JednLokalnaOsPrawnejPkd )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDaneOgolne ( Bir11OsFizycznaDaneOgolne )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDzialalnoscCeidg ( Bir11OsFizycznaDzialalnoscCeidg )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDzialalnoscPozostala ( Bir11OsFizycznaDzialalnoscPozostala )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDzialalnoscRolnicza ( Bir11OsFizycznaDzialalnoscRolnicza )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaDzialalnoscSkreslonaDo20141108 ( Bir11OsFizycznaDzialalnoscSkreslonaDo20141108 )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaListaJednLokalnych ( Bir11OsFizycznaListaJednLokalnych )
import Web.BIR.BIR11.Types.Report.Bir11OsFizycznaPkd ( Bir11OsFizycznaPkd )
import Web.BIR.BIR11.Types.Report.Bir11OsPrawna ( Bir11OsPrawna )
import Web.BIR.BIR11.Types.Report.Bir11OsPrawnaListaJednLokalnych ( Bir11OsPrawnaListaJednLokalnych )
import Web.BIR.BIR11.Types.Report.Bir11OsPrawnaPkd ( Bir11OsPrawnaPkd )
import Web.BIR.BIR11.Types.Report.Bir11OsPrawnaSpCywilnaWspolnicy ( Bir11OsPrawnaSpCywilnaWspolnicy )
import Web.BIR.BIR11.Types.Report.Bir11TypPodmiotu ( Bir11TypPodmiotu )

import Web.BIR.BIR11.Types ( Regon(unRegon), Regon14(unRegon14), Regon9(unRegon9) )


data Bir11FullReport :: Type -> Type where
  Bir11FrJednLokalnaOsFizycznej :: Regon14 -> Bir11FullReport Bir11JednLokalnaOsFizycznej
  Bir11FrJednLokalnaOsFizycznejPkd :: Regon14 -> Bir11FullReport [Bir11JednLokalnaOsFizycznejPkd]
  Bir11FrJednLokalnaOsPrawnej :: Regon14 -> Bir11FullReport Bir11JednLokalnaOsPrawnej
  Bir11FrJednLokalnaOsPrawnejPkd :: Regon14 -> Bir11FullReport [Bir11JednLokalnaOsPrawnejPkd]
  Bir11FrOsFizycznaDaneOgolne :: Regon9 -> Bir11FullReport Bir11OsFizycznaDaneOgolne
  Bir11FrOsFizycznaDzialalnoscCeidg :: Regon9 -> Bir11FullReport Bir11OsFizycznaDzialalnoscCeidg
  Bir11FrOsFizycznaDzialalnoscPozostala :: Regon9 -> Bir11FullReport Bir11OsFizycznaDzialalnoscPozostala
  Bir11FrOsFizycznaDzialalnoscRolnicza :: Regon9 -> Bir11FullReport Bir11OsFizycznaDzialalnoscRolnicza
  Bir11FrOsFizycznaDzialalnoscSkreslonaDo20141108 :: Regon9 -> Bir11FullReport Bir11OsFizycznaDzialalnoscSkreslonaDo20141108
  Bir11FrOsFizycznaPkd :: Regon9 -> Bir11FullReport [Bir11OsFizycznaPkd]
  Bir11FrOsFizycznaListaJednLokalnych :: Regon9 -> Bir11FullReport [Bir11OsFizycznaListaJednLokalnych]
  Bir11FrOsPrawna :: Regon9 -> Bir11FullReport Bir11OsPrawna
  Bir11FrOsPrawnaListaJednLokalnych :: Regon9 -> Bir11FullReport [Bir11OsPrawnaListaJednLokalnych]
  Bir11FrOsPrawnaPkd :: Regon9 -> Bir11FullReport [Bir11OsPrawnaPkd]
  Bir11FrOsPrawnaSpCywilnaWspolnicy :: Regon9 -> Bir11FullReport [Bir11OsPrawnaSpCywilnaWspolnicy]
  Bir11FrTypPodmiotu :: Regon -> Bir11FullReport Bir11TypPodmiotu

bir11FullReportToParams :: Bir11FullReport a -> (Text, Text)
bir11FullReportToParams = \case
  Bir11FrJednLokalnaOsFizycznej r14 -> ("BIR11JednLokalnaOsFizycznej", unRegon14 r14)
  Bir11FrJednLokalnaOsFizycznejPkd r14 -> ("BIR11JednLokalnaOsFizycznejPkd", unRegon14 r14)
  Bir11FrJednLokalnaOsPrawnej r14 -> ("BIR11JednLokalnaOsPrawnej", unRegon14 r14)
  Bir11FrJednLokalnaOsPrawnejPkd r14 -> ("BIR11JednLokalnaOsPrawnejPkd", unRegon14 r14)
  Bir11FrOsFizycznaDaneOgolne r9 -> ("BIR11OsFizycznaDaneOgolne", unRegon9 r9)
  Bir11FrOsFizycznaDzialalnoscCeidg r9 -> ("BIR11OsFizycznaDzialalnoscCeidg", unRegon9 r9)
  Bir11FrOsFizycznaDzialalnoscPozostala r9 -> ("BIR11OsFizycznaDzialalnoscPozostala", unRegon9 r9)
  Bir11FrOsFizycznaDzialalnoscRolnicza r9 -> ("BIR11OsFizycznaDzialalnoscRolnicza", unRegon9 r9)
  Bir11FrOsFizycznaDzialalnoscSkreslonaDo20141108 r9 -> ("BIR11OsFizycznaDzialalnoscSkreslonaDo20141108", unRegon9 r9)
  Bir11FrOsFizycznaListaJednLokalnych r9 -> ("BIR11OsFizycznaListaJednLokalnych", unRegon9 r9)
  Bir11FrOsFizycznaPkd r9 -> ("BIR11OsFizycznaPkd", unRegon9 r9)
  Bir11FrOsPrawna r9 -> ("BIR11OsPrawna", unRegon9 r9)
  Bir11FrOsPrawnaListaJednLokalnych r9 -> ("BIR11OsPrawnaListaJednLokalnych", unRegon9 r9)
  Bir11FrOsPrawnaPkd r9 -> ("BIR11OsPrawnaPkd", unRegon9 r9)
  Bir11FrOsPrawnaSpCywilnaWspolnicy r9 -> ("BIR11OsPrawnaSpCywilnaWspolnicy", unRegon9 r9)
  Bir11FrTypPodmiotu r -> ("BIR11TypPodmiotu", unRegon r)
