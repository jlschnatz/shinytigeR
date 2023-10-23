

write_userdata_db <- function(id_user, .response_data_df, .drv = RSQLite::SQLite(), .db = "db_user.sqlite") {
    pool <- pool::dbPool(drv = .drv, dbname = .db)
    user_vec <- pool::dbListTables(pool)

    if (!id_user %in% user_vec) {
      pool::dbCreateTable(
        conn = pool,
        name = id_user,
        fields = colnames(.response_data_df)
        )
    }

    pool::dbAppendTable(conn = pool, name = id_user, value = .response_data_df)
    pool::poolClose(pool)
}






