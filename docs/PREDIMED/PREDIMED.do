data gcat2; set gcat2;
if PREDIMED_ACEITE_GRASA = "1" then pred_agrasa=1;
	else if PREDIMED_ACEITE_GRASA = "2" then pred_agrasa=0;
	else if PREDIMED_ACEITE_GRASA = "0" then pred_agrasa=.;

if PREDIMED_ACEITE_CONSUMO = 1 then pred_acons=1;
	else if PREDIMED_ACEITE_CONSUMO = 2 then pred_acons=0;
	else if PREDIMED_ACEITE_CONSUMO = 0 then pred_acons=.;

if PREDIMED_VERDURAS = "1" then pred_verd=1;
	else if PREDIMED_VERDURAS = "2" then pred_verd=0;
	else if PREDIMED_VERDURAS = "0" then pred_verd=.;

if PREDIMED_FRUTA = "1" then pred_frut=1;
	else if PREDIMED_FRUTA = "2" then pred_frut=0;
	else if PREDIMED_FRUTA = "0" then pred_frut=.;

if PREDIMED_CARNES_ROJAS = "1" then pred_croja=0;
	else if PREDIMED_CARNES_ROJAS = "2" then pred_croja=1;
	else if PREDIMED_CARNES_ROJAS = "0" then pred_croja=.;
	
if PREDIMED_MANTEQUILLA = "1" then pred_mant=0;
	else if PREDIMED_MANTEQUILLA = "2" then pred_mant=1;
	else if PREDIMED_MANTEQUILLA = "0" then pred_mant=.;
	
if PREDIMED_BEBIDAS_CARBONATADAS  = "1" then pred_bcar=0;
	else if PREDIMED_BEBIDAS_CARBONATADAS  = "2" then pred_bcar=1;
	else if PREDIMED_BEBIDAS_CARBONATADAS  = "0" then pred_bcar=.;
	
if PREDIMED_VINO  = "1" then pred_vino=1;
	else if PREDIMED_VINO  = "2" then pred_vino=0;
	else if PREDIMED_VINO  = "0" then pred_vino=.;
	
if PREDIMED_LEGUMBRES = "1" then pred_leg=1;
	else if PREDIMED_LEGUMBRES = "2" then pred_leg=0;
	else if PREDIMED_LEGUMBRES = "0" then pred_leg=.;
	
if PREDIMED_PESCADO = "1" then pred_pesc=1;
	else if PREDIMED_PESCADO = "2" then pred_pesc=0;
	else if PREDIMED_PESCADO = "0" then pred_pesc=.;
	
if PREDIMED_REPOSTERIA = "1" then pred_repost=0;
	else if PREDIMED_REPOSTERIA = "2" then pred_repost=1;
	else if PREDIMED_REPOSTERIA = "0" then pred_repost=.;
	
if PREDIMED_FRUTOS_SECOS = "1" then pred_fsecos=1;
	else if PREDIMED_FRUTOS_SECOS = "2" then pred_fsecos=0;
	else if PREDIMED_FRUTOS_SECOS = "0" then pred_fsecos=.;
	
if PREDIMED_CARNE  = "1" then pred_carne=1;
	else if PREDIMED_CARNE  = "2" then pred_carne=0;
	else if PREDIMED_CARNE  = "0" then pred_carne=.;
	
if PREDIMED_VEGETALES = "1" then pred_veg=1;
	else if PREDIMED_VEGETALES = "2" then pred_veg=0;
	else if PREDIMED_VEGETALES = "0" then pred_veg=.;
run;
data gcat2; set gcat2;
mdscore =(pred_veg + pred_carne + pred_fsecos + pred_repost + pred_pesc + pred_leg + pred_vino + pred_bcar + pred_mant + pred_croja + pred_frut + pred_verd + pred_acons + pred_agrasa);
mdpredimedcat = mdscore;
	if mdscore = . then mdpredimedcat=99;
		else if mdscore =<5 then mdpredimedcat =0;
		else if mdscore >5 and mdscore =<9 then mdpredimedcat = 1;
		else if mdscore >9 then mdpredimedcat = 2;
run;

data gcat2; set gcat2;
if entity_id = "=E00251415430721" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251427367821" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251427353121" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251428427021" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251510045021" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251513783321" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251510616321" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251537618521" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251527352321" then CALC_AVG_PESO_p= 67;
if entnty_id = "=E00251517794121" then CALC_AVG_PESO_p= 67;
if entity_id = "=E00251517794121" then CALC_AVG_PESO_p= 67;/**/
if entity_id = "=E00251511135221" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251524970121" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251537618621" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251533032721" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251527354321" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251527371121" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251617236821" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251616667621" then CALC_AVG_PESO_p= 83;
if entity_id = "=E00251415410321" then CALC_AVG_HEIGHT_p=174;
if entity_id = "=E00251512722421" then CALC_AVG_HEIGHT_p=174;
if entity_id = "=E00251517794121" then CALC_AVG_HEIGHT_p=160;
run;
data gcat2; set gcat2;
if entity_id = "=E00251428929621" then CALC_AVG_CADERA_p = 103.5;
if entity_id = "=E00251428929621" then CALC_AVG_CINTURA_p =96.55;
if entity_id = "=E00251428924621" then CALC_AVG_CADERA_p =103.5;
if entity_id = "=E00251428924621" then CALC_AVG_CINTURA_p =96.55;/*TOP MEN*/
if entity_id = "=E00251428924721" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251428924721" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251428929421" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251428929421" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251428930621" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251428930621" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251428953021" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251428953021" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251415211321" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251415211321" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251428908821" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251428908821" then CALC_AVG_CADERA_p = 85;
if entity_id = "=E00251537601121" then CALC_AVG_CADERA_p = 103;
if entity_id = "=E00251537601121" then CALC_AVG_CADERA_p = 85;

whr=CALC_AVG_CINTURA_p/CALC_AVG_CADERA_p; 
run;