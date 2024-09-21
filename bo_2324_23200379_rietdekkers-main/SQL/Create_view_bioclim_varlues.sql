CREATE OR REPLACE VIEW public.bioclim_values
AS SELECT o.end_dev_eui,
    f.fieldlab_name,
    EXTRACT(year FROM o.time_stamp) AS observation_year,
    avg(
        CASE
            WHEN o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio1_t,
    max(
        CASE
            WHEN o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) - min(
        CASE
            WHEN o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio2_t,
    stddev_pop(
        CASE
            WHEN o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) * 100::double precision AS bio4_t,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[9::numeric, 10::numeric, 11::numeric])) AND o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio8_t,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[3::numeric, 4::numeric, 5::numeric])) AND o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio9_t,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[6::numeric, 7::numeric, 8::numeric])) AND o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio10_t,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[12::numeric, 1::numeric, 2::numeric])) AND o.parameter_name = 'TempC_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio11_t,
    avg(
        CASE
            WHEN o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio1_h,
    max(
        CASE
            WHEN o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) - min(
        CASE
            WHEN o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio2_h,
    stddev_pop(
        CASE
            WHEN o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) * 100::double precision AS bio4_h,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[9::numeric, 10::numeric, 11::numeric])) AND o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio8_h,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[3::numeric, 4::numeric, 5::numeric])) AND o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio9_h,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[6::numeric, 7::numeric, 8::numeric])) AND o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio10_h,
    avg(
        CASE
            WHEN (EXTRACT(month FROM o.time_stamp) = ANY (ARRAY[12::numeric, 1::numeric, 2::numeric])) AND o.parameter_name = 'Hum_SHT'::text AND o.value <= 105::double precision THEN o.value
            ELSE NULL::double precision
        END) AS bio11_h
   FROM fieldlab f
     JOIN fieldlab_device fd ON fd.fieldlab_id = f.fieldlab_id
     JOIN observation o ON fd.end_dev_eui = o.end_dev_eui
  WHERE f.fieldlab_name <> 'HAS test'::text
  GROUP BY (EXTRACT(year FROM o.time_stamp)), f.fieldlab_name, o.end_dev_eui;