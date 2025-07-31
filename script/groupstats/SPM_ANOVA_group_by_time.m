%-----------------------------------------------------------------------
% Job saved on 06-Dec-2018 13:27:25 by cfg_util (rev $Rev: 6942 $)
% spm SPM - SPM12 (7219)
% cfg_basicio BasicIO - Unknown
%-----------------------------------------------------------------------
% res_dir = '/Users/yuanzh/Desktop/groupstats_mathFUN_taskfmri/anova_group_by_time/rsa_dot_num_near_vs_far_group_by_time';
addpath('/software/spm12/')
res_dir = '/projects/yunjip/2024_md_td_tutoring/results/taskfmri/groupstats/rsa/anova_group_by_time_pediatric';
mkdir(res_dir);

matlabbatch{1}.spm.stats.factorial_design.dir = {res_dir};
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(1).name = 'subjects';
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(1).dept = 0; % independent
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(1).variance = 0; % equal variance
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(1).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(1).ancova = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(2).name = 'group';
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(2).dept = 0; % independent
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(2).variance = 1; % unequal variance
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(2).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(2).ancova = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(3).name = 'time';
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(3).dept = 1; % 1: non-independent between factor levels
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(3).variance = 0;% 0 : equal variance
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(3).gmsca = 0;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.fac(3).ancova = 0;
%%
subjlists= {'/projects/yunjip/2024_md_td_tutoring/data/subjectlist/group_20TD_pre.csv', ...
            '/projects/yunjip/2024_md_td_tutoring/data/subjectlist/group_15MD_pre_without8015.csv', ...
            '/projects/yunjip/2024_md_td_tutoring/data/subjectlist/group_20TD_post.csv', ...
            '/projects/yunjip/2024_md_td_tutoring/data/subjectlist/group_15MD_post_without8015.csv'};



slist_g1t1 = csvread(subjlists{1},1); % TD, pre
slist_g1t2 = csvread(subjlists{3},1); % TD, post
slist_g1 = [slist_g1t1; slist_g1t2];
slist_g2t1 = csvread(subjlists{2},1); % MD, pre
slist_g2t2 = csvread(subjlists{4},1); % MD, post
slist_g2 = [slist_g2t1; slist_g2t2];

idx = 1;
prefix = '/projects/yunjip/2024_md_td_tutoring/results/taskfmri/participants/';
scans = {};

for gp = 1:2
    if gp == 1
        nsub = length(slist_g1);
        slist = sortrows(slist_g1,1);
    else 
        nsub = length(slist_g2);
        slist = sortrows(slist_g2,1);
    end   
    
    for s = 1:nsub
         scans{idx,1} = fullfile(prefix, num2str(slist(s,1)), ['visit' num2str(slist(s,2))], ['session' num2str(slist(s,3))], ...
                 'rsa/stats_spm12/11_all_near_vs_all_far_comp_dot_VS_11_all_near_vs_all_far_comp_num_pediatric/rsa_zscore.nii,1');

        idx = idx + 1;
    end
    
end

matlabbatch{1}.spm.stats.factorial_design.des.fblock.fsuball.specall.scans = scans;

%%
numsub = [20 15];
numgroup = 2;
numtime = 2;
imatrix = [];

for g = 1:numgroup
    if g == 1
        ss = 1;
        ee = numsub(g);
    else
        ss = numsub(1) + 1;
        ee = ss + numsub(2) - 1;
    end 
    for s = ss:ee
        for t = 1:numtime
            imatrix = [imatrix; 1, s, g, t];
        end
    end
end

matlabbatch{1}.spm.stats.factorial_design.des.fblock.fsuball.specall.imatrix = imatrix;

%%
matlabbatch{1}.spm.stats.factorial_design.des.fblock.maininters{1}.fmain.fnum = 1;
matlabbatch{1}.spm.stats.factorial_design.des.fblock.maininters{2}.inter.fnums = [2
                                                                                  3];

%%
matlabbatch{1}.spm.stats.factorial_design.cov = struct('c', {}, 'cname', {}, 'iCFI', {}, 'iCC', {});
matlabbatch{1}.spm.stats.factorial_design.multi_cov = struct('files', {}, 'iCFI', {}, 'iCC', {});
matlabbatch{1}.spm.stats.factorial_design.masking.tm.tm_none = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.im = 1;
matlabbatch{1}.spm.stats.factorial_design.masking.em = {''};
matlabbatch{1}.spm.stats.factorial_design.globalc.g_omit = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.gmsca.gmsca_no = 1;
matlabbatch{1}.spm.stats.factorial_design.globalm.glonorm = 1;

%%
matlabbatch{2}.spm.stats.fmri_est.spmmat{1} = fullfile(res_dir,'SPM.mat');
matlabbatch{2}.spm.stats.fmri_est.method.Classical = 1;

matlabbatch{3}.spm.stats.con.spmmat = {fullfile(res_dir, 'SPM.mat')};
matlabbatch{3}.spm.stats.con.consess{1}.tcon.name = 'TD > MD';
matlabbatch{3}.spm.stats.con.consess{1}.tcon.weights = [0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05	0.05  -0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667	-0.0666666666666667 0.5 0.5 -0.5 -0.5];
matlabbatch{3}.spm.stats.con.consess{1}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.consess{2}.tcon.name = 'MD > TD';
matlabbatch{3}.spm.stats.con.consess{2}.tcon.weights = [-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05	-0.05 0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667	0.0666666666666667 -0.5 -0.5 0.5 0.5];
matlabbatch{3}.spm.stats.con.consess{2}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.consess{3}.tcon.name = 'Post > Pre';
matlabbatch{3}.spm.stats.con.consess{3}.tcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.5 0.5 -0.5 0.5];
matlabbatch{3}.spm.stats.con.consess{3}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.consess{4}.tcon.name = 'Pre > Post';
matlabbatch{3}.spm.stats.con.consess{4}.tcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.5 -0.5 0.5 -0.5];
matlabbatch{3}.spm.stats.con.consess{4}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.consess{5}.tcon.name = 'TD (post > pre) > MD (post > pre)';
matlabbatch{3}.spm.stats.con.consess{5}.tcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -0.5 0.5 0.5 -0.5];
matlabbatch{3}.spm.stats.con.consess{5}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.consess{6}.tcon.name = 'MD (post > pre) > TD (post > pre)';
matlabbatch{3}.spm.stats.con.consess{6}.tcon.weights = [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.5 -0.5 -0.5 0.5];
matlabbatch{3}.spm.stats.con.consess{6}.tcon.sessrep = 'none';
matlabbatch{3}.spm.stats.con.delete = 1;

BatchFile = fullfile(res_dir, 'batch_anova.mat');
save(BatchFile,'matlabbatch');

spm_jobman('initcfg');
delete(get(0, 'Children'));
spm_jobman('run', BatchFile);