CREATE OR REPLACE VIEW public.view_graphs AS
SELECT f.fieldlab_name,
       fd.tag,
       o.time_stamp,
       p.parameter_description,
       o.value,
       fd.schimmel,
       fd.hoogte,
       fd.orientatie,
       fd.constructie
FROM fieldlab f
JOIN fieldlab_device fd ON fd.fieldlab_id = f.fieldlab_id
JOIN observation o ON fd.end_dev_eui = o.end_dev_eui
JOIN parameter p ON o.parameter_name = p.parameter_name
WHERE (o.parameter_name = 'TempC_SHT' OR o.parameter_name = 'Hum_SHT')
  AND p.model_id = 'lsn50v2-s31'
  AND o.value < 105
  AND o.time_stamp >= fd.start_date
  AND o.time_stamp <= COALESCE(fd.end_date, CURRENT_DATE + 1)
  AND f.fieldlab_name <> 'HAS test';