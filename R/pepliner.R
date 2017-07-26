suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(lazyeval))
suppressPackageStartupMessages(library(crayon))
suppressPackageStartupMessages(library(seqinr))


options(tibble.width=Inf)


# create parser object
arguments <- function(){
    parser <- ArgumentParser()
    #Tcell_2502_Cyto_sequence_annotated.csv
    parser$add_argument('-f','--filename',dest='filename', help ='Input parsed .csv elution profile file',required=TRUE)
    parser$add_argument('-p','--proteome',dest='proteome', help ='Input .fasta file containing sequences of the analyzed proteins',default='')
    parser$add_argument('-o','--outname',dest='outname', help ='Input name of the folder to store the output and suffix of the created files',required=TRUE)
    parser$add_argument('-l','--list',dest='protein_list',nargs='*', help = 'Input list of object IDs to run through the pipeline. Default option runs all the objects.',default='')
    parser$add_argument('-id','--group',dest='groupid', help = 'Name of the group ID column',default='ProteinID')
    parser$add_argument('-e','--element',dest='elementid', help = 'Name of the element ID column',default='Peptide')
    parser$add_argument('-x','--xaxis',dest='xaxis', help = 'Name of the column to plot as the x-axis of the sparklines',default='FractionID')
    parser$add_argument('-y','--yaxis',dest='yaxis', help = 'Name of the column to plot as the y-axis of the sparklines',default='PeptideCount')
    parser$add_argument('-c','--conditions',dest = 'condit', help = '[OPTIONAL] Add this flag if the experimental data contain elutions under different conditions, specified under a column named "Condition"', default ='')
    parser$add_argument('-a','--annotations',dest='annot', help = 'Input tab-separated proteome annotation file where with the first column named "Entry".',default='')
    
    args <- parser$parse_args()
    return(args)
}


normalit<-function(m){
    (m - min(m))/(max(m)-min(m))
}

sparkline <- function(z,groupid,elementid,xaxis,yaxis,condit,annot){
    #groupid <- as.name(groupid)
    groupid <- as.name(groupid)
    xaxis <- as.name(xaxis)
    
    if(condit!=''){
        condition <- as.name(condit)
    }else{condition<-as.name(groupid)}
    
    z2 <- z %>% group_by_(condition)
    z2 <- z2 %>% mutate_(.dots = setNames(list(interp(~normalit(var),var=as_name(yaxis))),yaxis))
    z2 <- z2 %>% arrange_(.dots=yaxis)
    z2[yaxis][is.na(z2[yaxis])] <- 0
    
    #If there is one group 
    
    
    plt <- ggplot(z2, aes_string(x=xaxis, y=yaxis, group=condition, color=condition, linetype=condition)) +
        geom_line(size=0.8, position=position_dodge(width=0.25), alpha=0.9) +
        scale_color_manual(values=c("#74add1","#a50026","#fee090","#f46d43","#4575b4")) +
        scale_linetype_manual(values=c("solid","twodash","F1","dotted","dotdash")) +
        theme(legend.position="None")
    
    final_plt <- ggplotGrob(plt)
    
    
    
    #remove unnecessary plot elements
    #print(final_plt)
    
    final_plt <- gtable_remove_grobs(final_plt, c('title','subtitle','caption','spacer', 'xlab-b','ylab-l', 'axis-b','axis-l','spacer'))
    
    #print(final_plt)
    #compress unused plot space
    #print(unique(z$Peptide))
    final_plt <- gtable_squash_rows(final_plt, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    final_plt <- gtable_squash_cols(final_plt, c(1, 2, 3, 5, 6, 7))
    
    
    #print(plot_grid(final_plt))
    return(final_plt)
}

rectangles <- function(row,elementid){
    # row1 = data[1:12,]
    # row2
    row$dummy_y <- 20
    
    rect <- ggplot(data = row, aes(x=max(row$Length), y = dummy_y)) +
        theme(axis.text = element_text(size=8)) +
        geom_blank() +
        theme(
            axis.text.y=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.line.y=element_blank(),
            axis.line.x=element_blank()) +
        geom_rect(aes(xmin=0, xmax=max(row$Length), ymin=0, ymax=20,fill=I('grey95'))) +
        geom_vline(xintercept=seq(0, max(row$Length), by=100),color='grey56') +
        geom_rect(aes(xmin=row$Start, xmax=row$End, ymin=0, ymax=20,fill=I('#f46d43'))) +
        geom_text(aes(label=as.character(head(select_(row,elementid)[,1],1))),size=4, position = position_nudge(x=-0.5*(max(row$Length)),y=-10))#as.character(head(row$Peptide,1)))
    
    rect <- ggplotGrob(rect)
    
    
    #remove unnecessary plot elements
    rect <- gtable_remove_grobs(rect, c('title', 'xlab-b', 'ylab-l', 'axis-b','axis-l','spacer'))
    #print(rect)
    #compress unused plot space
    rect <- gtable_squash_rows(rect, c(1, 2, 3, 4, 5, 7, 8, 9, 10))
    rect <- gtable_squash_cols(rect, c(1, 2, 3, 5, 6, 7))
    return(rect)
    
    #facet_grid(Peptide ~ .)
}


#Tcell_2502_Cyto_sequence_annotated.csv

proc_data <- function(raw_data,proteome,groupid,elementid,xaxis,yaxis,condit){
    data_raw <- raw_data
    #data_raw <- read.csv(filename, sep=",", header=TRUE)
    data_raw$Sequence <- NULL
    data_raw$PeptideArea <- NULL
    
    data_spread <- data_raw %>% spread_(xaxis, yaxis) 
    
    if(condit!=''){
        data <- data_spread %>% gather_(xaxis, yaxis, names(.)[!names(.)%in%c(groupid,elementid,'Start','End','Length','Appearance',condit)])
    }else{
        #todo: drop this line!
        data_spread <- data_spread[, (colnames(data_spread)!=as_name('ExperimentID'))]
        data <- data_spread %>% gather_(xaxis, yaxis, names(.)[!names(.)%in%c(groupid,elementid,'Start','End','Length','Appearance')])
    }
    
    data[is.na(data)] <- 0
    
    data<-unique(data) 
    
    
    #Don't want proteins that are too long :(
    if(proteome!=''){ 
        trimmed_data <- data %>% filter(Length < 1500)
        trimmed_data %>% group_by_(groupid) %>% dplyr::summarize(n_of_elements=n()) -> data2
    }
    
    #find the proteins with most unique peptides identified
    data %>% group_by_(groupid) %>% dplyr::summarize(n_of_elements=n()) -> data2
    
    #reorder this temporary data set by number of unique peptides
    data2[names(data2)==groupid] <- reorder(data2[names(data2)==groupid],data2$n_of_elements)
    #todo

    #Select proteins with multiple peptides (tail of 500)
    ordered_data <- tail(data2[order(data.frame(data2$n_of_elements)),][names(data2[order(data.frame(data2$n_of_elements)),])==groupid],500) 
    ordered_data = droplevels(ordered_data)
    head(ordered_data)
    
    output_list <- list("data" = data, "priority_list" = ordered_data)
    return(output_list)
}

cov_columns <- function(data_table,elementid){
    pep <- data_table %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='I',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>% mutate_(.dots = setNames(list(interp(~gsub(pattern='L',replacement='J',x=var),var=as_name(elementid))),elementid))
    pep <- pep %>%  mutate_(.dots = setNames(list(interp(~gsub(pattern='J',replacement='(I|L)',x=var),var=as_name(elementid))),elementid))
    peptide_column <- pep[,colnames(pep)==elementid]
    sequence_column <- pep[,colnames(pep)=='Sequence'] %>% as.character()
    Length <- nchar(sequence_column)
    
    Start <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 0){
            Start[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]) == 1){
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1]
        }else{
            Start[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1] %>% list()
        }
    }
    Start <- as.integer(Start)
    
    End <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 0){
            End[i] <- 'Not found'
        }else if(length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]) == 1){
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2]
        }else{
            End[i] <- str_locate_all(sequence_column[i],peptide_column[i])[[1]][,2] %>% list()
        }}
    End <- as.integer(End)
    
    
    Appearance <- rep('',length(peptide_column))
    for(i in 1:length(peptide_column)){
        Appearance[i] <- length(str_locate_all(sequence_column[i],peptide_column[i])[[1]][,1])
    }
    Appearance <- as.integer(Appearance)
    
    
    return(data.frame(Length,Start,End,Appearance))
}

pipeline <- function(protein_list,filename,proteome,outname,groupid,elementid,xaxis,yaxis,condit,annot){
    raw_data <- read.csv(filename, sep=",", header=TRUE)
    
    if(condit!=''){
        raw_data <- raw_data[colnames(raw_data) %in% c(groupid,elementid,xaxis,yaxis,condit)] #only use the required columns
    }else{
        raw_data <- raw_data[colnames(raw_data) %in% c(groupid,elementid,xaxis,yaxis)]
    }
    if(proteome!=''){
        fasta <- read.fasta(proteome)
        list_seq <- lapply(fasta,function(lst){lst[1:length(lst)] %>% paste(collapse='') %>% toupper()})
        list_seq %>% names() %>% length() %>% print()
        list_seq <- data.frame(names(list_seq) %>% as_vector(),unname(list_seq) %>% as_vector())
        names(list_seq) <- c(groupid,'Sequence')
        
        tmp <- merge(x = raw_data, y = list_seq, by = "ID", all.x = TRUE) 
        out_table <- cbind(tmp,cov_columns(tmp,elementid))
        out_table <- out_table[colnames(out_table)%in%c(groupid,elementid,xaxis,yaxis,condit,'Length','Sequence','Start','End','Appearance')]
        raw_data <- out_table

    }
    data_proteome <- proc_data(raw_data,proteome,groupid,elementid,xaxis,yaxis,condit)$data
    if(protein_list==''){
        protein_list <- as.vector(unique(data_proteome[colnames(data_proteome)==groupid]))[[1]]
    }
    #protein_list <- list('O00154')

   
    for(protein_name in protein_list){ #test proteins
        #print(paste('banana',protein_name))
        # data_protein <- data %>% filter(ProteinID==protein_name) #Find the caspase ENS Id for a good example of split peptides
        #data_protein <- proc_data(filename,groupid,condit)$data %>% filter_(interp(~colm==protein_name),colm=as.name(groupid))  #filter for a single protein in a list
        data_protein <- data_proteome[data_proteome[colnames(data_proteome)==groupid]==protein_name,] #not elegant, but hey! {base}
        #print(data_protein)
        
        #filter for a single protein in a list
        #To do: order n term to c term using start and end
        cat(paste('\nProcessing',protein_name))

        #Make a wide matrix from the tidy data
        data_wide <- spread_(data_protein, xaxis, yaxis)
        
        #Fill in NA with 0's to com plete the matrix
        data_wide[is.na(data_wide)] <- 0
        

        #Turn the wide matrix tidy.
        final_prot <- data_wide %>% gather_(xaxis, yaxis, names(.)[!names(.)%in%c(groupid,elementid,'Start','End','Length','Appearance',condit)]) 
        
        if(proteome!=''){
            final_prot <- final_prot[order(data.frame(final_prot$Start)),]#final_prot$Start[1]),]
            final_prot[colnames(final_prot)==elementid] <- reorder(select_(final_prot,elementid)[,1], final_prot$Start)
        }
        
        
        #Feed peptides into sparkline function
        #print(final_prot)
        final_prot = droplevels(final_prot)
        cat(paste('\nGroup ID column:',groupid %>% bold(),'\n'))
        cat(paste('Element ID column:',elementid %>% bold(),'\n'))
        cat(paste('X-axis column:',xaxis %>% bold(),'\n'))
        cat(paste('Y-axis column:',yaxis %>% bold(),'\n'))
        if(condit!=''){
            cat(paste('Label sparklines according to:',condit %>% bold(),'\n'))
        }
        if(annot!=''){
            cat(paste('Annotate figures according to:',annot %>% bold(),'\n'))
        }
        if(proteome!=''){
            cat(paste('Analyze protein coverage according to:', proteome %>% bold(),'\n'))    
        }
        
        
        final_prot %>% split(select_(final_prot,elementid)[,1]) %>% map(sparkline,groupid=groupid,elementid=elementid,xaxis=xaxis,yaxis=yaxis,condit=condit,annot=annot) -> plotlist2
        
        #Plot ggplot objects
        clusterplot <- plot_grid(plotlist = plotlist2, ncol=1, align = "v")
        #clusterplot
        
        if(proteome!=''){
            final_prot  %>% split(select_(final_prot,elementid)[,1]) %>% map(rectangles,elementid) -> listrects
            coverage <- plot_grid(plotlist = listrects, ncol=1, align = "v")
        }
        
        #Open human proteome annotations
        if(annot!=''){
            hum_annot <- read.csv(annot, sep="\t", header=TRUE)
            hum_annot[names(hum_annot)==groupid] <- hum_annot['Entry']
            genename<- hum_annot %>% filter(Entry == protein_name) %>% select(Gene.names...primary..)
            desc<- hum_annot %>% filter(Entry == protein_name) %>% select(Protein.names)
            subheader <- ggdraw() + draw_label(as.character(desc[1,1]),size=8)
            title <-ggdraw() + draw_label(paste(protein_name, as.character(genename[1,1])),size=20)
            figname <- paste(outname,'/figure_',gsub(protein_name,pattern='|','_'), "_", as.character(genename[1,1]), '_', outname, '.png',sep='')
            
            if(proteome!=''){
                figure <-  plot_grid(clusterplot, coverage, nrow=1)
            }else{
                figure <- clusterplot
            }
            
            figure_with_title <- plot_grid(title,subheader, figure,nrow=3,rel_heights = c(0.05,0.03, 1))
        }else{
            title <-ggdraw() + draw_label(protein_name,size=20)
            figname <- paste(outname,'/figure_', gsub(protein_name,pattern='|','_'), '_',outname,'.png',sep='')
            
            if(proteome!=''){
                figure <-  plot_grid(clusterplot, coverage, nrow=1)
            }else{
                figure <- clusterplot
            }
            figure_with_title <- plot_grid(title,figure,nrow=3,rel_heights = c(0.05,1))
            #substr(filename,1,nchar(filename)-4)
        }

        
        cat(paste('\nOutput file:',bold(blue(figname)),'\n'))
        png(file=figname, width=10, height=10, units="in", res = 100)
        print(figure_with_title)
        dev.off()  }
}


    
pepline <- function(){
    options(error=function()traceback(2))
    args <- arguments()
    if(!dir.exists(args$outname)){
        dir.create(args$outname)
    }
    pipeline(protein_list=args$protein_list,args$filename,args$proteome,args$outname,args$groupid,args$elementid,args$xaxis,args$yaxis,args$condit,args$annot)
    
    cat('\nAll done!\n')
}

if (length(grep("--file=pepline.R", commandArgs())) > 0){
    pepline()
}
