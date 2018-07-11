Unprotect[DateObject];
DateObject[date_List, _String, cal_String, tz_, ___] := DateObject[date, CalendarType -> cal, TimeZone -> tz]
Protect[DateObject];