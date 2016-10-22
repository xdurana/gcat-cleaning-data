var pred_agrasa = parseInt($('PREDIMED_ACEITE_GRASA').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_acons = parseInt($('PREDIMED_ACEITE_CONSUMO').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_verd = parseInt($('PREDIMED_VERDURAS').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_frut = parseInt($('PREDIMED_FRUTA').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_croja = parseInt($('PREDIMED_CARNES_ROJAS').map(
  {'0':0,
   '1':0,
   '2':1}, 0))

var pred_mant = parseInt($('PREDIMED_MANTEQUILLA').map(
  {'0':0,
   '1':0,
   '2':1}, 0))

var pred_bcar = parseInt($('PREDIMED_BEBIDAS_CARBONATADAS').map(
  {'0':0,
   '1':0,
   '2':1}, 0))

var pred_vino = parseInt($('PREDIMED_VINO').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_leg = parseInt($('PREDIMED_LEGUMBRES').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_pesc = parseInt($('PREDIMED_PESCADO').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_repost = parseInt($('PREDIMED_REPOSTERIA').map(
  {'0':0,
   '1':0,
   '2':1}, 0))

var pred_fsecos = parseInt($('PREDIMED_FRUTOS_SECOS').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_carne = parseInt($('PREDIMED_CARNE').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var pred_veg = parseInt($('PREDIMED_VEGETALES').map(
  {'0':0,
   '1':1,
   '2':0}, 0))

var mdscore = pred_veg + pred_carne + pred_fsecos + pred_repost + pred_pesc + pred_leg + pred_vino + pred_bcar + pred_mant + pred_croja + pred_frut + pred_verd + pred_acons + pred_agrasa

mdscore