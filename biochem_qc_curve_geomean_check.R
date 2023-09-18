x <- c(21.108562,
19.096904,
17.854187,
31.765871,
14.549786,
15.385488,
13.533886,
15.815289,
11.215128,
27.139048)


x <- c(0.000000021108562,
       0.000000019096904,
       0.000000017854187,
       0.000000031765871,
       0.000000014549786,
       0.000000015385488,
       0.000000013533886,
       0.000000015815289,
       0.000000011215128,
       0.000000027139048)

mean(x[1:4])

logs <- sapply(x[1:4], function(x) log(x,base =10))

10^(mean(logs))
    

logs <- sapply(x[5:length(x)], function(x) log(x,base =10))
10^(mean(logs))


sql <- "SELECT
    t1.batch_id  AS batch_id,
    t1.graph     AS graph,
    t1.ic50_nm   AS ic50_nm,
    t2.geo_nm    AS geo_nm,
    '-3 stdev: '
    || round(t2.nm_minus_3_stdev, 1)
    || '<br />'
    || '+3 stdev: '
    || round(t2.nm_plus_3_stdev, 1)
    || '<br />'
    || 'n of m: '
    || t2.n_of_m AS agg_stats,
    'CRO: '
    || t1.cro
    || '<br />'
    || 'Assay Type: '
    || t1.assay_type
    || '<br />'
    || 'Target: '
    || t1.target
    || '<br />'
    || 'Variant: '
    || t1.variant
    || '<br />'
    || 'cofactor-1: '
    || t1.cofactor_1
    || '<br />'
    || 'cofactor-2: '
    || t1.cofactor_2
    || 'atp_conc_um: '
    || t2.atp_conc_um
    || CHR(10)   AS properties
FROM
    (
        SELECT
            substr(t1.id, 0, 8)                                                       AS compound_id,
            t1.id                                                                     AS batch_id,
            nvl(t1.result_alpha, to_char(round((t1.result_numeric * 1000000000), 2))) AS ic50_nm,
            t2.file_blob                                                              AS graph,
            t4.cro,
            t5.assay_type,
            target,
            variant,
            cofactor_1,
            cofactor_2,
            atp_conc_um,
            t1.prop1
        FROM
                 ds3_userdata.tm_conclusions t1
            INNER JOIN ds3_userdata.tm_graphs t2 ON t1.experiment_id = t2.experiment_id
                AND t1.id = t2.id
                AND t1.prop1 = t2.prop1
            INNER JOIN (
                SELECT
                    experiment_id,
                    sample_id,
                    prop1,
                    MAX(decode(property_name, 'Target', property_value))              AS target,
                    nvl(MAX(decode(property_name, 'Variant', property_value)), '-')   AS variant,
                    MAX(decode(property_name, 'Cofactor-1', property_value))          AS cofactor_1,
                    MAX(decode(property_name, 'Cofactor-2', property_value))          AS cofactor_2,
                    MAX(decode(property_name, 'ATP Conc (uM)', property_value))       AS atp_conc_um
                FROM
                    ds3_userdata.tm_pes_fields_values
                WHERE
                        experiment_id = '-PRIMARY-'
                    AND sample_id != 'BLANK'
                GROUP BY
                    experiment_id,
                    sample_id,
                    prop1
            ) t3 ON t1.experiment_id = t3.experiment_id
                    AND t1.id = t3.sample_id
                    AND t1.prop1 = t3.prop1
            INNER JOIN (
                SELECT
                    experiment_id,
                    property_value AS cro
                FROM
                    ds3_userdata.tm_prot_exp_fields_values
                WHERE
                        experiment_id = '-PRIMARY-'
                    AND property_name = 'CRO'
            )                      t4 ON t1.experiment_id = t4.experiment_id
            INNER JOIN (
                SELECT
                    experiment_id,
                    property_value AS assay_type
                FROM
                    ds3_userdata.tm_prot_exp_fields_values
                WHERE
                        experiment_id = '-PRIMARY-'
                    AND property_name = 'Assay Type'
            )                      t5 ON t1.experiment_id = t5.experiment_id
        WHERE
            t1.experiment_id = '-PRIMARY-'
    ) t1
    LEFT OUTER JOIN ds3_userdata.ft_biochem_drc_stats t2 
                 ON t1.compound_id = t2.compound_id
                 AND t1.cro = t2.cro
                 AND t1.assay_type = t2.assay_type
                 AND t1.target = t2.target
                 AND t1.variant = t2.variant
                 --AND t1.cofactors = t2.cofactors
                 AND t1.atp_conc_um = t2.atp_conc_um
ORDER BY
    t1.batch_id,
    t1.prop1
"
gsub("[\r\n\t\v\f ]", " ", sql, fixed=T)

st <- "SELECT t1.batch_id  AS batch_id,    t1.graph     AS graph,    t1.ic50_nm   AS ic50_nm,    t2.geo_nm    AS geo_nm,    '-3 stdev: '    || round(t2.nm_minus_3_stdev, 1)    || '<br />'    || '+3 stdev: '    || round(t2.nm_plus_3_stdev, 1)    || '<br />'    || 'n of m: '    || t2.n_of_m AS agg_stats,    'CRO: '    || t1.cro    || '<br />'    || 'Assay Type: '    || t1.assay_type    || '<br />'    || 'Target: '    || t1.target    || '<br />'    || 'Variant: '    || t1.variant    || '<br />'    || 'cofactor-1: '    || t1.cofactor_1    || '<br />'    || 'cofactor-2: '    || t1.cofactor_2    || 'atp_conc_um: '    || t2.atp_conc_um    || CHR(10)   AS propertiesFROM    (        SELECT            substr(t1.id, 0, 8)                                                       AS compound_id,            t1.id                                                                     AS batch_id,            nvl(t1.result_alpha, to_char(round((t1.result_numeric * 1000000000), 2))) AS ic50_nm,            t2.file_blob                                                              AS graph,            t4.cro,            t5.assay_type,            target,            variant,            cofactor_1,            cofactor_2,"

