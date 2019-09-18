SELECT DISTINCT

CPP_1.EVENT_ID AS EVENT_ID,
CPP_1.POT_ID AS POT_ID, 
  
  NZ(DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32"),0)
	/
	DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1")
	AS propLgCnt_awl,
	
  NZ(DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= 32"),0)
	/
	DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND CARAPACE_LENGTH_MM >= -1")
	AS propLgWt_awl,	

  NZ(DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND FK_SEX_CODE = '2'"),0)
	/
	DSum("[FREQUENCY]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND FK_SEX_CODE IN ('1','2','3','9')")
	AS propFemCnt_awl,
	
  NZ(DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND FK_SEX_CODE = '2'"),0)
	/
	DSum("[KG]", "[CPP_4]", "[EVENT_ID] = '" & [EVENT_ID] & "' 
	AND [POT_ID] = '" & [POT_ID] & "'
	AND FK_SEX_CODE IN ('1','2','3','9')")
	AS propFemWt_awl	
	
FROM CPP_1

LEFT JOIN
CPP_4 ON (CPP_1.POT_ID = CPP_4.POT_ID)
 AND (CPP_1.EVENT_ID = CPP_4.EVENT_ID)

WHERE (((CPP_1.FK_GEAR_PERFORMANCE_CODE)="01"));