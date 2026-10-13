# Toy table
conv_tbl <- data.frame(
        mm.ens = c("ENSMUSG0001", "ENSMUSG0002"),
        hs.ens = c("ENSG0001", "ENSG0002"),
        mgi = c("GeneA", "GeneB"),
        hgnc = c("GENEA", "GENEB"),
        stringsAsFactors = FALSE
    )

test_that("gene conversion functions run", {
    conv_tbl <- data.frame(
        mm.ens = c("ENSMUSG0001", "ENSMUSG0002"),
        hs.ens = c("ENSG0001", "ENSG0002"),
        mgi = c("GeneA", "GeneB"),
        hgnc = c("GENEA", "GENEB"),
        stringsAsFactors = FALSE
    )

    expect_no_error(table_convert_genes(
        genes = c("ENSG0001", "ENSG0002"),
        from = "ENSG",
        to = "HGNC",
        conversion_table = conv_tbl
    ))

    expect_no_error(table_convert_genes(
        genes = c("ENSMUSG0001", "ENSMUSG0002"),
        from = "ENSMUSG",
        to = "MGI",
        conversion_table = conv_tbl[ , c("mm.ens", "mgi")]
    ))

})

test_that("gene conversion function handles incorrect inputs", {
    expect_error(table_convert_genes(
        genes = c("ENSG0001", "ENSG0002"),
        from = "ENSG",
        to = "MGI",
        conversion_table = conv_tbl[ , c("hgnc", "mgi")]
    ), "Conversion table must contain columns for hs.ens, mgi")

    expect_error(table_convert_genes(
        genes = c("ENSG0001", "ENSG0002"),
        from = "ENSG",
        to = "HGNC",
        conversion_table = setNames(conv_tbl, c("ENSMUSG", "ENSG", "MGI", "HGNC"))
    ), "Conversion table must contain columns for hs.ens, hgnc")

    expect_error(table_convert_genes(
        genes = c("ENSG0001", "ENSG0002"),
        from = "ENSG",
        to = "ZFIN",
        conversion_table = conv_tbl
    ), "All values in to must be: MGI, HGNC"
    )
})
