rm -rf src/turb_test
cp -r src/turb src/turb_test
python3 inspect_mod.py ./src/turb_test

for f in \
ec_lun.F90      \
ec_parkind.F90      \
fxtran.conf      \
fxtran.conf.bk      \
get_time.c      \
les_mean_subgrid_phy.F90      \
main_turb.F90      \
modd_argslist_ll.F90      \
modd_budget.F90      \
modd_cst.F90      \
modd_cturb.F90      \
modd_dimphyex.F90      \
modd_field.F90      \
modd_io.F90      \
modd_les.F90      \
modd_neb_n.F90      \
modd_parameters.F90      \
modd_turb_n.F90      \
modi_get_halo.F90      \
modi_second_mnh.F90      \
modi_turb.F90      \
parkind1.F90      \
turb.F90      \
turb.intfb.h      \
util_cst_t_mod.F90      \
util_csturb_t_mod.F90      \
util_dimphyex_t_mod.F90      \
util_neb_t_mod.F90      \
util_tfiledata_mod.F90      \
util_tles_t_mod.F90      \
util_turb_t_mod.F90      \
values      \
xrd_getoptions.F90      \
xrd_unix_env.F90      \
yomdata.F90      \
yomdata.fypp      \
yomhook.F90      \
mode_bl89.F90      \
mode_cloud_modif_lm.F90      \
mode_compute_function_thermo.F90      \
mode_compute_function_thermo_new_stat.F90      \
mode_dear.F90      \
mode_delt.F90      \
mode_emoist.F90      \
mode_etheta.F90      \
mode_gradient_m_phy.F90      \
mode_gradient_u_phy.F90      \
mode_gradient_v_phy.F90      \
mode_gradient_w_phy.F90      \
mode_ibm_mixinglength.F90      \
mode_io_field_write_phy.F90      \
mode_mppdb.F90      \
mode_prandtl.F90      \
mode_sbl_phy.F90      \
mode_shuman_phy.F90      \
mode_sources_neg_correct.F90      \
mode_tke_eps_sources.F90      \
mode_tools.F90      \
mode_tridiag_thermo.F90      \
mode_tridiag_tke.F90      \
mode_tridiag_wind.F90      \
mode_turb_ver_dyn_flux.F90      \
mode_turb_ver.F90      \
mode_turb_ver_sv_corr.F90      \
mode_turb_ver_thermo_corr.F90      \
mode_turb_ver_thermo_flux.F90      \
mode_update_iiju_phy.F90      
do
echo "==>" $f "<=="
file=./src/turb/$f #files before transformation
test_ref_file=./src/turb_test_ref/$f #file to which compare
test_file=./src/turb_test/$f #file created when running the test
diff $test_ref_file $test_file
done
