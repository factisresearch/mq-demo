-- -*- coding: utf-8 -*-
{-# LANGUAGE OverloadedStrings #-}
module Mgw.I18n.Main where

import Mgw.I18n.Core

import qualified Data.Text as T

tasksStr :: T.Text
tasksStr =
    case _LANG_ of
      German -> "Aufgaben"
      English -> "Tasks"

openTaskStr :: T.Text
openTaskStr = switchLang "offen" "open"

openLaterTaskStr :: T.Text
openLaterTaskStr = switchLang "später" "later"

doneTaskStr :: T.Text
doneTaskStr = switchLang "erledigt" "done"

dueTaskStr :: T.Text
dueTaskStr = switchLang "fällig" "due"

openTaskByPatStr :: T.Text
openTaskByPatStr = switchLang "offen/Patient" "open/pat"

openTaskByCatStr :: T.Text
openTaskByCatStr = switchLang "offen/Art" "open/kind"

finishedTaskStr :: T.Text
finishedTaskStr = switchLang "bearbeitet" "finished"

synchronizationStr :: T.Text
synchronizationStr = switchLang "Synchronisation" "Synchronization"

emptyBedStr :: T.Text
emptyBedStr = switchLang "leeres Bett" "Empty Bed"

unknownRoomStr :: T.Text
unknownRoomStr = switchLang "Zimmer unbekannt" "Unknown room"

reportsStr :: T.Text
reportsStr = switchLang "Befunde" "Reports"

otherStr :: T.Text
otherStr = switchLang "Sonstiges" "Miscellaneous"

colleaguesOnlineStr :: T.Text
colleaguesOnlineStr = switchLang "Kollegen online" "Colleagues Online"

myColleaguesStr :: T.Text
myColleaguesStr = switchLang "Meine Kollegen" "My Colleagues"

lastUseStr :: T.Text
lastUseStr = switchLang "Letzte Nutzung" "Last Use"

onlineSinceStr :: T.Text
onlineSinceStr = switchLang "online seit" "online since"

offlineSinceStr :: T.Text
offlineSinceStr = switchLang "offline ab" "offline since"

myPatientsStr :: T.Text
myPatientsStr = switchLang "meine Patienten" "My Patients"

otherPatientsStr :: T.Text
otherPatientsStr = switchLang "andere Patienten" "Other Patients"

outpatientsStr :: T.Text
outpatientsStr = switchLang "Aussenlieger" "Outpatients"

workflowStr :: T.Text
workflowStr = switchLang "Workflow" "Workflow"

firstVisitStr :: T.Text
firstVisitStr = switchLang "Erstbesuch beim Patienten" "Initial Visitation"

lastThreeDaysStr :: T.Text
lastThreeDaysStr = switchLang "Letzten 3 Tage" "Last 3 Days"

neverStr :: T.Text
neverStr = switchLang "nie" "never"

xDaysAgoStr :: Int -> T.Text
xDaysAgoStr d =
    case _LANG_ of
      German -> T.pack $ "vor " ++ daysStr ++ " Tagen"
      English -> T.pack $ daysStr ++ " days ago"
    where daysStr = show d

docsStr :: T.Text
docsStr =
    case _LANG_ of
      German -> "Dokumente "
      English -> "Documents "

picturesStr :: T.Text
picturesStr =
    case _LANG_ of
      German -> "Bilddokumentation"
      English -> "Pictures"

audioRecStr :: T.Text
audioRecStr =
    case _LANG_ of
      German -> "Tonaufnahme"
      English -> "Audio Recordings"

settingsStr :: T.Text
settingsStr =
    case _LANG_ of
      German -> "Einstellungen"
      English -> "Settings"

myPatsStr :: T.Text
myPatsStr =
    case _LANG_ of
      German -> "persönlich"
      English -> "My Patients"

myPatsByNameStr :: T.Text
myPatsByNameStr =
    case _LANG_ of
      German -> "pers. A-Z"
      English -> "My Pats. A-Z"

myPatsByRoomStr :: T.Text
myPatsByRoomStr =
    case _LANG_ of
      German -> "pers. 0-9"
      English -> "My Pats. 0-9"

myTeamStr :: T.Text
myTeamStr =
    case _LANG_ of
      German -> "mein Team"
      English -> "My Team"

myTeamByNameStr :: T.Text
myTeamByNameStr =
    case _LANG_ of
      German -> "Team A-Z"
      English -> "Team A-Z"

myTeamByRoomStr :: T.Text
myTeamByRoomStr =
    case _LANG_ of
      German -> "Team 0-9"
      English -> "Team 0-9"

otherTeamsStr :: T.Text
otherTeamsStr =
    case _LANG_ of
      German -> "andere Teams"
      English -> "Other Teams"

otherTeamsByNameStr :: T.Text
otherTeamsByNameStr =
    case _LANG_ of
      German -> "andere Teams A-Z"
      English -> "Other Teams A-Z"

otherTeamsByRoomStr :: T.Text
otherTeamsByRoomStr =
    case _LANG_ of
      German -> "andere Teams 0-9"
      English -> "Other Teams 0-9"

allPatsStr :: T.Text
allPatsStr =
    case _LANG_ of
      German -> "alle"
      English -> "All"

commonTaskItemStr :: T.Text
commonTaskItemStr =
    case _LANG_ of
      German -> "Aufgaben Person"
      English -> "Tasks Person"


tasksStationStr :: T.Text
tasksStationStr =
    case _LANG_ of
      German -> "Aufgaben Station"
      English -> "Tasks Ward"

adtStr :: T.Text
adtStr =
    case _LANG_ of
      German -> "Bewegungen"
      English -> "ADT"

patsStr :: T.Text
patsStr =
    case _LANG_ of
      German -> "Patienten"
      English -> "Patients"

patsByNameStr :: T.Text
patsByNameStr =
    case _LANG_ of
      German -> "Patienten A-Z"
      English -> "Patients A-Z"

patsByRoomStr :: T.Text
patsByRoomStr =
    case _LANG_ of
      German -> "Patienten 0-9"
      English -> "Patients 0-9"

persPatsStr :: T.Text
persPatsStr =
    case _LANG_ of
      German -> "ausgewählte Patienten"
      English -> "specific patients"

myTasksStr :: T.Text
myTasksStr =
    case _LANG_ of
      German -> "meine Aufgaben"
      English -> "My Tasks"

myOrdersStr :: T.Text
myOrdersStr =
    case _LANG_ of
      German -> "meine Aufträge"
      English -> "My Orders"

tasksByPersonStr :: T.Text
tasksByPersonStr =
    case _LANG_ of
      German -> "Aufgaben nach Person"
      English -> "In Charge"

ordersByPersonStr :: T.Text
ordersByPersonStr =
    case _LANG_ of
      German -> "Aufträge nach Person"
      English -> "Ordered By"

inChargeOfStr :: T.Text
inChargeOfStr =
    case _LANG_ of
      German -> "Aufgaben für "
      English -> "In charge of "

orderedByStr :: T.Text
orderedByStr =
    case _LANG_ of
      German -> "Aufträge von "
      English -> "Ordered by "

unsignedRepsStr :: T.Text
unsignedRepsStr =
    case _LANG_ of
      German -> "ungekürzelte Befunde"
      English -> "Unsigned Reports"

myUnsignedRepsStr :: T.Text
myUnsignedRepsStr =
    case _LANG_ of
      German -> "meine ungekürzelten Befunde"
      English -> "My Unsigned Reports"

moreStr :: T.Text
moreStr =
    case _LANG_ of
      German -> "mehr..."
      English -> "More..."

toBeSignedStr :: T.Text
toBeSignedStr =
    case _LANG_ of
      German -> "zu kürzelnde Befunde"
      English -> "To Be Signed"

reportsByUpdateStr :: T.Text
reportsByUpdateStr =
    case _LANG_ of
      German -> "Befunde (nach Eingang)"
      English -> "Reports (Update Order)"

reportsByObservationStr :: T.Text
reportsByObservationStr =
    case _LANG_ of
      German -> "Befunde (nach Beobachtung)"
      English -> "Reports (Observation Order)"

markedReportsStr :: T.Text
markedReportsStr =
    case _LANG_ of
      German -> "markierte Befunde"
      English -> "Marked Reports"

markedDocsStr :: T.Text
markedDocsStr =
    case _LANG_ of
      German -> "markierte Dokumente"
      English -> "Marked Documents"

plannedStr :: T.Text
plannedStr =
    case _LANG_ of
      German -> "geplant"
      English -> "Planned"

todayStr :: T.Text
todayStr =
    case _LANG_ of
      German -> "heute"
      English -> "Today"

yesterdayStr :: T.Text
yesterdayStr =
    case _LANG_ of
      German -> "gestern"
      English -> "Yesterday"

laskWeekStr :: T.Text
laskWeekStr =
    case _LANG_ of
      German -> "letzte Woche"
      English -> "Last Week"

olderStr :: T.Text
olderStr =
    case _LANG_ of
      German -> "älter"
      English -> "Older"

labviewerStr :: T.Text
labviewerStr =
    case _LANG_ of
      German -> "Laborviewer"
      English -> "Labviewer"

trendStr :: T.Text
trendStr = "Trend"

labvaluesStr :: T.Text
labvaluesStr =
    case _LANG_ of
      German -> "Wichtige Laborwerte"
      English -> "Important Labs"

personalStr :: T.Text
personalStr =
    case _LANG_ of
      German -> "persönlich"
      English -> "Personal"


dateDdMmFormat :: String
dateDdMmFormat =
    case _LANG_ of
      German -> "%d.%m"
      English -> "%m/%d"

shortDateFormat :: String
shortDateFormat =
    case _LANG_ of
      German -> "%d.%m.%y"
      English -> "%y/%m/%d"

longDateFormat :: String
longDateFormat =
    case _LANG_ of
      German -> "%d.%m.%Y"
      English -> "%Y/%m/%d"

timeFormat :: String
timeFormat =
    case _LANG_ of
      German -> "%H:%M"
      English -> "%-l:%M%P"

timeFormatSeconds :: String
timeFormatSeconds =
    case _LANG_ of
      German -> "%H:%M:%S"
      English -> "%-l:%M%S%P"

unknownStr :: T.Text
unknownStr =
    case _LANG_ of
      German -> "unbekannt"
      English -> "unknown"

roomStr :: T.Text
roomStr =
    case _LANG_ of
      German -> "Zi.: "
      English -> "R: "

visitAdmissionStr :: T.Text
visitAdmissionStr =
    case _LANG_ of
      German -> "A: "
      English -> "Admission: "

medRepOrderReceivedStr :: T.Text
medRepOrderReceivedStr =
    case _LANG_ of
      German -> "Auftrag eingegangen"
      English -> "order received"

medRepProceedingStr :: T.Text
medRepProceedingStr =
    case _LANG_ of
      German -> "in Bearbeitung"
      English -> "in process"

medRepPreliminaryStr :: T.Text
medRepPreliminaryStr =
    case _LANG_ of
      German -> "vorläufig"
      English -> "preliminary"

medRepCorrectionStr :: T.Text
medRepCorrectionStr =
    case _LANG_ of
      German -> "korrigiert"
      English -> "correction"

medRepStoredStr :: T.Text
medRepStoredStr =
    case _LANG_ of
      German -> "fertig, aber ungeprüft"
      English -> "stored"

medRepFinalStr :: T.Text
medRepFinalStr =
    case _LANG_ of
      German -> "endgültig"
      English -> "final"

repDateIncomingStr :: T.Text
repDateIncomingStr =
    case _LANG_ of
      German -> "E "
      English -> "I "


repDateObservationStr :: T.Text
repDateObservationStr =
    case _LANG_ of
      German -> "B "
      English -> "O "

prioVeryHighStr :: T.Text
prioVeryHighStr =
    case _LANG_ of
      German -> "sehr hoch"
      English -> "very high"

prioHighStr :: T.Text
prioHighStr =
    case _LANG_ of
      German -> "hoch"
      English -> "high"

prioRegularStr :: T.Text
prioRegularStr =
    case _LANG_ of
      German -> "normal"
      English -> "regular"

prioLowStr :: T.Text
prioLowStr =
    case _LANG_ of
      German -> "niedrig"
      English -> "low"

prioVeryLowStr :: T.Text
prioVeryLowStr =
    case _LANG_ of
      German -> "sehr niedrig"
      English -> "very low"

admissionsStr :: T.Text
admissionsStr =
    case _LANG_ of
      German -> "Aufnahmen"
      English -> "Admissions"

dischargesStr :: T.Text
dischargesStr =
    case _LANG_ of
      German -> "Entlassungen"
      English -> "Discharges"

movedStr :: T.Text
movedStr =
    case _LANG_ of
      German -> "Verlegungen"
      English -> "Transfers"

lastStr :: T.Text
lastStr =
    case _LANG_ of
      German -> "letzte "
      English -> "last "

daysStr :: T.Text
daysStr =
    case _LANG_ of
      German -> "Tage"
      English -> "days"

subscribedReportsStr :: T.Text
subscribedReportsStr =
    case _LANG_ of
      German -> "abonnierte Befunde"
      English -> "Subscribed Reports"

subscribedStr :: T.Text
subscribedStr =
    case _LANG_ of
      German -> "abonniert"
      English -> "subscribe"

newReportsStr :: T.Text
newReportsStr =
    case _LANG_ of
      German -> "neue Befunde"
      English -> "New Reports"

newStr :: T.Text
newStr =
    case _LANG_ of
      German -> "neu"
      English -> "new"

birthStr :: T.Text
birthStr =
    case _LANG_ of
      German -> "Geburtsdatum"
      English -> "Birthdate"

wardStr :: T.Text
wardStr =
    case _LANG_ of
      German -> "Station"
      English -> "Ward"

insuranceStr :: T.Text
insuranceStr =
    case _LANG_ of
      German -> "Versicherung"
      English -> "Insurance"

insStateStr :: T.Text
insStateStr =
    case _LANG_ of
      German -> "Versicherungsstatus"
      English -> "State of insurance"

addrStr :: T.Text
addrStr =
    case _LANG_ of
      German -> "Adresse"
      English -> "Address"

admDateStr :: T.Text
admDateStr =
    case _LANG_ of
      German -> "Aufnahmedatum"
      English -> "Admission date"


disDateStr :: T.Text
disDateStr =
    case _LANG_ of
      German -> "Entlassdatum"
      English -> "Discharge date"

stayStr :: T.Text
stayStr =
    case _LANG_ of
      German -> "Aufenthalt"
      English -> "Stay"

doctorStr :: T.Text
doctorStr =
    case _LANG_ of
      German -> "Arzt"
      English -> "Doctor"

doctorsStr :: T.Text
doctorsStr =
    case _LANG_ of
      German -> "Ärzte"
      English -> "Doctors"

refDoctorStr :: T.Text
refDoctorStr =
    case _LANG_ of
      German -> "einweisender Arzt"
      English -> "referring physician"

relativesStr :: T.Text
relativesStr =
    case _LANG_ of
      German -> "Angehörige"
      English -> "Relatives"

otherConsStr :: T.Text
otherConsStr =
    case _LANG_ of
      German -> "weitere Kontakte"
      English -> "Other contacts"

diagsStr :: T.Text
diagsStr =
    case _LANG_ of
      German -> "Diagnosen"
      English -> "Diagnoses"

diagnosisStr :: T.Text
diagnosisStr =
    case _LANG_ of
      German -> "Diagnose"
      English -> "Diagnosis"

encodeDiagnosesStr :: T.Text
encodeDiagnosesStr =
    case _LANG_ of
      German -> "Diagnosen kodieren"
      English -> "Encode Diagnoses"

notesStr :: T.Text
notesStr =
    case _LANG_ of
      German -> "Notiz"
      English -> "Notes"

noDiagsStr :: T.Text
noDiagsStr =
    case _LANG_ of
      German -> "keine Diagnosen vorhanden"
      English -> "no diagnoses available"

procsStr :: T.Text
procsStr =
    case _LANG_ of
      German -> "Therapien"
      English -> "Therapies"

diagsProcsStr :: T.Text
diagsProcsStr =
    case _LANG_ of
      German -> "Diagnose/Therapie"
      English -> "Diagnosis/Therapy"

noProcsStr :: T.Text
noProcsStr =
    case _LANG_ of
      German -> "keine Prozeduren vorhanden"
      English -> "no procedures available"

masterDataStr :: T.Text
masterDataStr =
    case _LANG_ of
      German -> "Stammdaten"
      English -> "Master data"

generalStr :: T.Text
generalStr =
    case _LANG_ of
      German -> "Allgemein"
      English -> "General"

estimPriceStr :: T.Text
estimPriceStr =
    case _LANG_ of
      German -> "geschätzter Preis"
      English -> "estimated price"

orderingProvStr :: T.Text
orderingProvStr =
    case _LANG_ of
      German -> "Auftraggeber"
      English -> "ordering provider"

recDateStr :: T.Text
recDateStr =
    case _LANG_ of
      German -> "Eingang"
      English -> "received date"

obsDateStr :: T.Text
obsDateStr =
    case _LANG_ of
      German -> "Beobachtung"
      English -> "observation date"

stateStr :: T.Text
stateStr =
    case _LANG_ of
      German -> "Status"
      English -> "state"


createdStr :: T.Text
createdStr =
    case _LANG_ of
      German -> "erstellt am"
      English -> "created on"


orgIdStr :: T.Text
orgIdStr =
    case _LANG_ of
      German -> "Original-ID"
      English -> "original ID"

labStr :: T.Text
labStr =
    case _LANG_ of
      German -> "Labor"
      English -> "lab"

labOrderStr :: T.Text
labOrderStr =
    case _LANG_ of
      German -> "Laborauftrag "
      English -> "lab order no "

privPatStr :: T.Text
privPatStr =
    case _LANG_ of
      German -> "Privatpatient"
      English -> "private patient"

semiPrivPatStr :: T.Text
semiPrivPatStr =
    case _LANG_ of
      German -> "Halbprivat"
      English -> "semi-private"

genInsPatStr :: T.Text
genInsPatStr =
    case _LANG_ of
      German -> "Allgemein"
      English -> "generic"

insSelfPayStr :: T.Text
insSelfPayStr =
    case _LANG_ of
      German -> "Selbstzahler"
      English -> "self pay patient"

insHeadPhysStr :: T.Text
insHeadPhysStr =
    case _LANG_ of
      German -> "Chefarztbehandlung"
      English -> "head physician treatment"

insSingleRoomStr :: T.Text
insSingleRoomStr =
    case _LANG_ of
      German -> "Einzelzimmer"
      English -> "single room"

materialStr :: T.Text
materialStr =
    case _LANG_ of
      German -> "Probe"
      English -> "material"

bgStr :: T.Text
bgStr =
    case _LANG_ of
      German -> "Berufsgenossenschaft"
      English -> "employer's insurance"

sliceThicknessStr :: T.Text
sliceThicknessStr =
    case _LANG_ of
      German -> "Schichtdicke"
      English -> "Thickness"
