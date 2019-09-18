SELECT 
CPP_3.YEAR,
CPP_3.EVENT_ID,
CPP_3.SITE_ID,
CPP_3.STATION,
CPP_3.POT_ID,
CPP_3.SAMPLE_POT,
CPP_3.all_Cnt as all_Cnt_cc,
CPP_3.all_Kg as all_Kg_cc,

IIf(CPP_3.all_Cnt = 0, 0,[CPP_3.all_Cnt] * [CPP_5.propLgCnt_awl]) AS lrg_Cnt,
IIf ([CPP_3.all_Kg] = 0, 0,[CPP_3.all_Kg] * [CPP_5.propLgWt_awl]) AS lrg_Kg,

IIf(CPP_3.all_Cnt = 0, 0,[CPP_3.all_Cnt] * [CPP_5.propFemCnt_awl]) AS fem_Cnt,
IIf ([CPP_3.all_Kg] = 0, 0,[CPP_3.all_Kg] * [CPP_5.propFemWt_awl]) AS fem_Kg

INTO CPP 

FROM (CPP_3
	LEFT JOIN
		CPP_5 ON (CPP_3.POT_ID = CPP_5.POT_ID)
		 AND (CPP_3.EVENT_ID = CPP_5.EVENT_ID))

ORDER BY    CPP_3.YEAR, CPP_3.SITE_ID, CPP_3.STATION, CPP_3.POT_ID 
;