# converter

# This script is converted from Theodore Beers' typescript version of the date converter,
# <https://raw.githubusercontent.com/theodore-s-beers/unical/main/unical.ts>
# ONLINE APPLICATION: https://www.muqawwim.com/
# accuracy it still not entirely clear...

# Wüstenfeld's Tables: https://archive.org/details/vergleichungstab01wust/mode/2up

# Vergleichungs -Tabellen der Muhammedanischen und Christlichen Zeitrechnung
# by Wüstenfeld, Ferdinand, 1808-1899; Mahler, Eduard, 1857-1945
# Fortsetzung der Wüstenfeld'schen Vergleichungs-Tabellen der Muhammedanischen und Christliche
# Zeitrechnung von 1300 bis 1500 der Hedschra; Deutsche Morgenländische Gesellschaft

# Intro from Wüstenfeld

# Da die Muhammedaner nach Mondjahren zu 354 Tagen rechnen, so ist ihr Jahr durchschnittlich um 11 Tage kürzer als unser Sonnenjahr, was auf hundert Jahre einen Unterschied von etwa drei Jahren ausmacht. Zur genauern Vergleichung dieser beiden Zeitrechnungen sind mun besonders in dem letzten Jahrhundert eine nicht unbedeutende Anzahl von Tabellen erschienen 1) und man hat auch verschiedene Methoden zu ihrer Berechnung bekannt gemacht 2). Das Verfahren hierbei ist indess theils nicht leicht im Gedächtniss zu behalten, wenn man nicht täglich damit umgeht, theils erfordert es einige Gewandtheit im Rechnen, theils ist es immer mit einigem Aufwande von Zeit verbunden, - Grund genug, warum es gewiss mur höchst selten von Gelehrten in Anwendung gebracht ist. Jene Tabellen aber waren zum Theil nicht jedem zugänglich, zum Theil waren sie nicht vollständig oder nicht zweckmässig geordnet, einige sogar fehlerhaft berechnet, und selbst die ausführlichsten beschränkten sich darauf, nur für den Anfang jedes Muhammedanischen Jahres das entsprechende Datum unsrer Zeitrechnung anzugeben. Deshalb hatte ich schon vor mehreren Jahren angefangen, zum eigenen Gebrauch etwas ausführlichere Tabellen anzufertigen und bin nun dem durch einen Freund, welcher davon Kenntniss erhielt, veranlassten Auftrage des Vorstandes der Deutschen Morgenländischen Gesellschaft bereitwillig nachgekommen, diese Tabellen bis auf die neueste Zeit fortzuführen und der Öffentlichkeit zu übergeben.
# Sie sind also zunächst, wie der Titel besagt, nach dem ersten Tage jedes Muhammedanischen Monats berechnet, so dass man aus ihnen z. B. sogleich ersehen kann, dass der 1. Ramadhan 151 dem 18. September 768 entspricht; zugleich weist das neben diesem Datum stehende A darauf hin, dass dieser Tag ein Sonntag war. Eine weitere Thätigkeit des Nachschlagenden findet dann mur noch für die Tage innerhalb eines Monates statt. Da es für manche Berechnung von Wichtigkeit ist, den so genannten SonntagsBuchstaben zu kennen, so ist dieser gleich vorn neben die Jahrszahl geselzt, wobei indess zu merken, dass der Anfang des betreffenden Christlichen Jahres in den meisten Fällen schon in die vorhergehende Reihe fällt und also der Sonntags- Buchstab auch rückwärts bis zu diesem Anfange gilt; z. B. für das Christliche Jahr 771 ist F als Sonntagsbuchstab neben dem Muhammedanischen Jahre 155 angemerkt, ungeachtet das Jahr 771 fast ganz mit dem Jahre 154 zusammenfällt.
# Da Christliche Schaltjahre alle diejenigen sind, welche sich mit 4 theilen lassen, ausgenommen 1700 und 1800, so war hier eine Bezeichnung unnötig; dagegen sind die Muhammedanischen Schaltjahre mit einem Sternchen versehen und zwar rechts, um darauf aufmerksam zu machen, dass der letzte Monat, neben welchem das Sternchen steht, 30 Tage habe, weil der Schalttag immer am Ende des Jahres hinzugerechnet wird.
# In den beiden Streitfragen, ob die Muhammedanische Zeitrechnung Donnerstag den 15. Juli oder Freilag den 16. Juli 622 beginne, und ob in dem 30jährigen Cyclus das 15. oder 16. Jahr ein Schaltjahr sei, habe ich mich jedesmal für die zweite Meinung entschieden.
# Die Einführung des Gregorianischen Calenders und die in Folge davon überschlagenen zehn Tage vom 5. bis 14. October 1582 sind an der betreffenden Stelle Seite 41 bemerklich gemacht und von da an die Berechnung nur nach dem Gregorianischen Calender forigeführt.

# TB START
# // This library was initially adapted from the work of John Walker. His license
# // is included below, at two points. As you can imagine, much has been changed
# // over the years...

# /*
#        JavaScript functions for the Fourmilab Calendar Converter

#                   by John Walker -- September, MIM
#               http://www.fourmilab.ch/documents/calendar/

#                 This program is in the public domain.
# */

################################################################################
# Equivalent of NORM_LEAP
################################################################################

NORM_LEAP <- c("Normal year", "Leap year")

################################################################################
# LEAP_GREGORIAN -- Is a given year in the Gregorian calendar a leap year?
################################################################################

leapGregorian <- function(year) {
  return(year %% 4 == 0 && !(year %% 100 == 0 && year %% 400 != 0))
}

################################################################################
# GREGORIAN_TO_JD -- Determine Julian day number from Gregorian calendar date
################################################################################

GREGORIAN_EPOCH <- 1721425.5

gregorianToJD <- function(year, month, day) {
  return(
    GREGORIAN_EPOCH -
      1 +
      365 * (year - 1) +
      floor((year - 1) / 4) -
      floor((year - 1) / 100) +
      floor((year - 1) / 400) +
      floor(
        (367 * month - 362) / 12 +
          (ifelse(month <= 2, 0, ifelse(leapGregorian(year), -1, -2))) +
          day
      )
  )
}


jdToGregorian <- function(jd) {

  mod <- function(x, y) {
    return(x %% y)
  }
  
  wjd <- floor(jd - 0.5) + 0.5
  dEpoch <- wjd - GREGORIAN_EPOCH
  quadricent <- floor(dEpoch / 146097)
  dqc <- mod(dEpoch, 146097)
  cent <- floor(dqc / 36524)
  dCent <- mod(dqc, 36524)
  quad <- floor(dCent / 1461)
  dQuad <- mod(dCent, 1461)
  yIndex <- floor(dQuad / 365)
  
  year <- quadricent * 400 + cent * 100 + quad * 4 + yIndex
  if (!(cent == 4 || yIndex == 4)) {
    year <- year + 1
  }
  
  yearDay <- wjd - gregorianToJD(year, 1, 1)
  leapAdj <- ifelse(wjd < gregorianToJD(year, 3, 1), 0, ifelse(leapGregorian(year), 1, 2))
  month <- floor(((yearDay + leapAdj) * 12 + 373) / 367)
  day <- wjd - gregorianToJD(year, month, 1) + 1
  
  return(c(year, month, day))
}

# JWDAY -- Calculate day of week from Julian day
jWeekday <- function(j) {
  mod <- function(x, y) {
    return(x %% y)
  }
  # adding 1, since R indexes start with 1, not 0
  return(mod(floor(j + 1.5), 7) + 1)
}


################################################################################
# JULIAN_TO_JD -- Determine Julian day number from Julian calendar date
################################################################################

# leapJulian Function - Determines if a year in the Julian calendar is a leap year
leapJulian <- function(year) {
  mod <- function(x, y) {
    return(x %% y)
  }
  
  if (year > 0) {
    return(mod(year, 4) == 0)
  } else {
    return(mod(year, 4) == 3)
  }
}

# julianToJD Function - Converts a date from the Julian calendar to Julian Day Number
julianToJD <- function(year, month, day) {
  # Adjust negative common era years to the zero-based notation we use.
  if (year < 1) {
    year <- year + 1
  }
  
  # Algorithm as given in Meeus, Astronomical Algorithms, chapter 7, page 61
  if (month <= 2) {
    year <- year - 1
    month <- month + 12
  }
  
  return (
    floor(365.25 * (year + 4716)) +
      floor(30.6001 * (month + 1)) +
      day -
      1524.5
  )
}


################################################################################
# JD_TO_JULIAN -- Calculate Julian calendar date from Julian day
################################################################################

jdToJulian <- function(td) {
  td <- td + 0.5
  z <- floor(td)

  a <- z
  b <- a + 1524
  c <- floor((b - 122.1) / 365.25)
  d <- floor(365.25 * c)
  e <- floor((b - d) / 30.6001)

  month <- floor(ifelse(e < 14, e - 1, e - 13))
  year <- floor(ifelse(month > 2, c - 4716, c - 4715))
  day <- b - d - floor(30.6001 * e)

  # If year is less than 1, subtract one to convert from
  # a zero based date system to the common era system in
  # which the year -1 (1 B.C.E) is followed by year 1 (1 C.E.).
  if (year < 1) {
    year <- year - 1
  }

  return(c(year, month, day))
}

################################################################################
# HEBREW_TO_JD -- Determine Julian day from Hebrew date
################################################################################

HEBREW_EPOCH <- 347995.5

# Is a given Hebrew year a leap year?
hebrewLeap <- function(year) {
  mod <- function(x, y) {
    return(x %% y)
  }
  
  return(mod(year * 7 + 1, 19) < 7)
}

# How many months are there in a Hebrew year (12 = normal, 13 = leap)
hebrewYearMonths <- function(year) {
  return(ifelse(hebrewLeap(year), 13, 12))
}

# Test for delay of start of new year and to avoid Sunday, Wednesday, and Friday as start of the new year.
hebrewDelay1 <- function(year) {
  months <- floor((235 * year - 234) / 19)
  parts <- 12084 + 13753 * months
  day <- months * 29 + floor(parts / 25920)
  
  if (mod(3 * (day + 1), 7) < 3) {
    day <- day + 1
  }
  return(day)
}

# Check for delay in start of new year due to length of adjacent years
hebrewDelay2 <- function(year) {
  last <- hebrewDelay1(year - 1)
  present <- hebrewDelay1(year)
  next <- hebrewDelay1(year + 1)

  return(ifelse(next - present == 356, 2, ifelse(present - last == 382, 1, 0)))
}

# How many days are in a Hebrew year?
hebrewYearDays <- function(year) {
  # hebrewToJD function needs to be defined
  return(hebrewToJD(year + 1, 7, 1) - hebrewToJD(year, 7, 1))
}


# How many days are in a given month of a given year
hebrewMonthDays <- function(year, month) {
  # Fixed-length 29 day months
  if (month %in% c(2, 4, 6, 10, 13)) {
    return(29)
  }

  # If it's not a leap year, Adar has 29 days
  if (month == 12 && !hebrewLeap(year)) {
    return(29)
  }

  # Heshvan days depend on length of year
  if (month == 8 && hebrewYearDays(year) %% 10 != 5) {
    return(29)
  }

  # Kislev varies with the length of year
  if (month == 9 && hebrewYearDays(year) %% 10 == 3) {
    return(29)
  }

  # Default to 30 day month
  return(30)
}

# Finally, wrap it all up into...
hebrewToJD <- function(year, month, day) {
  months <- hebrewYearMonths(year)
  jd <- HEBREW_EPOCH + hebrewDelay1(year) + hebrewDelay2(year) + day + 1

  if (month < 7) {
    for (mon in 7:months) {
      jd <- jd + hebrewMonthDays(year, mon)
    }
    for (mon in 1:(month - 1)) {
      jd <- jd + hebrewMonthDays(year, mon)
    }
  } else {
    for (mon in 7:(month - 1)) {
      jd <- jd + hebrewMonthDays(year, mon)
    }
  }

  return(jd)
}

# JD_TO_HEBREW -- Convert Julian date to Hebrew date

jdToHebrew <- function(jd) {
  jd <- floor(jd) + 0.5
  count <- floor(((jd - HEBREW_EPOCH) * 98496.0) / 35975351.0)
  year <- count - 1
  for (i in count:(count+100)) {  # Increase the upper limit if necessary
    if (jd < hebrewToJD(i, 7, 1)) {
      break
    }
    year <- year + 1
  }
  first <- ifelse(jd < hebrewToJD(year, 1, 1), 7, 1)
  month <- first
  for (i in first:(first+12)) {  # Assuming a year does not exceed 12 months
    if (jd <= hebrewToJD(year, i, hebrewMonthDays(year, i))) {
      break
    }
    month <- month + 1
  }
  day <- jd - hebrewToJD(year, month, 1) + 1
  return(c(year, month, day))
}

################################################################################
# ISLAMIC
################################################################################

# LEAP_ISLAMIC -- Is a given year a leap year in the Islamic calendar?

leapIslamic <- function(year) {
  return ((year * 11 + 14) %% 30 < 11)
}

# ISLAMIC_TO_JD -- Determine Julian day from Islamic date


ISLAMIC_EPOCH <- 1948439.5

# ISLAMIC_WEEKDAYS - Names of the days of the week in the Islamic calendar
ISLAMIC_WEEKDAYS <- c(
  'yawm al-aḥad (Sunday)',
  'yawm al-iṯnayn (Monday)',
  'yawm al-ṯulaṯāʾ (Tuesday)',
  'yawm al-arbiʿāʾ (Wednesday)',
  'yawm al-ḫamīs (Thursday)',
  'yawm al-jumʿaŧ (Friday)',
  'yawm al-sabt (Saturday)'
)

ISLAMIC_MONTHS_NAME_TO_NUMBER <- function(month_name) {
  month_map <- c(
    "Muharram" = 1,
    "محرم" = 1, 
    "Safar" = 2,
    "صفر" = 2, 
    "Rabi' al-awwal" = 3,
    "ربيع الأول" = 3,
    "Rabi' I" = 3,
    "Rabi' al-thani" = 4,
    "ربيع الثاني" = 4,
    "Rabi' II" = 4, 
    "Jumada al-awwal" = 5,
    "جمادى الأول" = 5,
    "Jumada I" = 5,
    "Jumada al-thani" = 6, 
    "جمادى الآخر" = 6, 
    "Jumada II" = 6, 
    "Rajab" = 7, 
    "رجب" = 7, 
    "Sha'ban" = 8, 
    "شعبان" = 8,
    "Ramadan" = 9, 
    "رمضان" = 9, 
    "Shawwal" = 10, 
    "شوال" = 10, 
    "Dhu al-Qi'dah" = 11, 
    "ذو القعدة" = 11, 
    "Dhu al-Hijjah" = 12, 
    "ذو الحجة" = 12)
  
  return(month_map[month_name])
}

islamicToJD <- function(year, month, day) {
  return (
    day +
    ceiling(29.5 * (month - 1)) +
    (year - 1) * 354 +
    floor((3 + 11 * year) / 30) +
    ISLAMIC_EPOCH -
    1
  )
}

jdToGregorian(islamicToJD(597, 1, 1))

# JD_TO_ISLAMIC -- Calculate Islamic date from Julian day

jdToIslamic <- function(jd) {
  jd <- floor(jd) + 0.5
  year <- floor((30 * (jd - ISLAMIC_EPOCH) + 10646) / 10631)
  month <- min(
    12,
    ceiling((jd - (29 + islamicToJD(year, 1, 1))) / 29.5) + 1
  )
  day <- jd - islamicToJD(year, month, 1) + 1
  return(c(year, month, day))
}

################################################################################
# IRANIAN
################################################################################

# TEHRAN_EQUINOX -- Determine Julian day and fraction of the March equinox at
#                   the Tehran meridian in a given Gregorian year.

tehranEquinox <- function(year) {
  # equinox, deltaT, equationOfTime functions must be defined
  
  # March equinox in dynamical time
  equJED <- equinox(year, 0)
  
  # Correct for delta T to obtain Universal time
  equJD <- equJED - deltaT(year) / (24 * 60 * 60)
  
  # Apply the equation of time to yield the apparent time at Greenwich
  equAPP <- equJD + equationOfTime(equJED)
  
  # Correction for the difference between the Greenwich meridian and the Iran Standard time zone
  dtTehran <- (52 + 30 / 60.0) / 360
  equTehran <- equAPP + dtTehran
  
  return(equTehran)
}

# TEHRAN_EQUINOX_JD -- Calculate Julian day during which the March equinox,
#                      reckoned from the Tehran meridian, occurred for a given Gregorian year.

tehranEquinoxJD <- function(year) {
  ep <- tehranEquinox(year)
  epg <- floor(ep)
  
  return(epg)
}

#     PERSIANA_YEAR -- Determine the year in the Persian
#                      astronomical calendar in which a
#                      given Julian day falls. Returns an
#                      array of two elements:
#
#                         [0] Persian year
#                         [1] Julian day number containing
#                             equinox for this year


# PERSIAN_WEEKDAYS - Names of the days of the week in the Persian calendar


PERSIAN_WEEKDAYS <- c(
  'Yekshanbeh',
  'Doshanbeh',
  'Sehshanbeh',
  'Chaharshanbeh',
  'Panjshanbeh',
  'Jom‘eh',
  'Shanbeh'
)

PERSIAN_EPOCH <- 1948320.5
TropicalYear <- 365.24219878  # This value might need adjustment based on specific astronomical definitions

# PERSIANA_YEAR -- Determine the year in the Persian astronomical calendar in which a given Julian day falls

persianAYear <- function(jd) {
  # jdToGregorian and tehranEquinoxJD functions must be defined

  # Start with an estimate of the Gregorian year
  guess <- jdToGregorian(jd)[1] - 2

  # Find the last equinox in or before the given Julian day
  lastEq <- tehranEquinoxJD(guess)
  while (lastEq > jd) {
    guess <- guess - 1
    lastEq <- tehranEquinoxJD(guess)
  }

  # Find the next equinox after the given Julian day
  nextEq <- lastEq - 1
  while (!(lastEq <= jd && jd < nextEq)) {
    lastEq <- nextEq
    guess <- guess + 1
    nextEq <- tehranEquinoxJD(guess)
  }

  # Calculate the Persian astronomical year
  adr <- round((lastEq - PERSIAN_EPOCH) / TropicalYear) + 1

  return(c(adr, lastEq))
}

# JD_TO_PERSIANA -- Calculate date in the Persian astronomical calendar from Julian day.

jdToPersianA <- function(jd) {
  jd <- floor(jd) + 0.5
  adr <- persianAYear(jd)
  year <- adr[1]
  equinox <- adr[2]
  
  yDay <- floor(jd) - persianAToJD(year, 1, 1) + 1
  month <- ifelse(yDay <= 186, ceiling(yDay / 31), ceiling((yDay - 6) / 30))
  day <- floor(jd) - persianAToJD(year, month, 1) + 1

  return(c(year, month, day))
}

# PERSIANA_TO_JD -- Obtain Julian day from a given Persian astronomical calendar date.
persianAToJD <- function(year, month, day) {
  guess <- PERSIAN_EPOCH - 1 + TropicalYear * (year - 1 - 1)
  adr <- c(year - 1, 0)

  while (adr[1] < year) {
    adr <- persianAYear(guess)
    guess <- adr[2] + (TropicalYear + 2)
  }
  equinox <- adr[2]

  jd <- equinox + (ifelse(month <= 7, (month - 1) * 31, (month - 1) * 30 + 6)) + (day - 1)
  return(jd)
}

# LEAP_PERSIANA -- Is a given year a leap year in the Persian astronomical calendar?
leapPersianA <- function(year) {
  return (persianAToJD(year + 1, 1, 1) - persianAToJD(year, 1, 1) > 365)
}

################################################################################
# TESTS
################################################################################

hijri_to_gregorian <- function(year, month, day) {
  ymd <- jdToGregorian(islamicToJD(year, month, day))
  dow <- jWeekday(islamicToJD(year, month, day))
  return(c(ymd, dow))
}

hijri_to_gregorian_text <- function(year, month, day) {
  ymd <- jdToGregorian(islamicToJD(year, month, day))
  dow <- jWeekday(islamicToJD(year, month, day))
  return(c(ymd, ISLAMIC_WEEKDAYS[dow]))
}

hijri_to_gregorian(597, 1, 1)
hijri_to_gregorian_text(597, 1, 1)
hijri_to_gregorian_text(656, 2, 15)
