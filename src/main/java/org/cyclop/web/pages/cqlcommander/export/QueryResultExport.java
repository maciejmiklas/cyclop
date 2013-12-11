package org.cyclop.web.pages.cqlcommander.export;

import java.text.SimpleDateFormat;
import java.util.Date;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.util.resource.IResourceStream;
import org.apache.wicket.util.resource.StringResourceStream;
import org.cyclop.common.AppConfig;
import org.cyclop.service.model.CqlQuery;
import org.cyclop.service.model.CqlSelectResult;
import org.cyclop.service.converter.CsvQueryResultExporter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Maciej Miklas
 */
public class QueryResultExport {

    private final static Logger LOG = LoggerFactory.getLogger(QueryResultExport.class);

    private final static String DATE_PATTERN = "yyyy-MM-dd HH:mm:ss.SSS";

    private final Downloader downloader;

    private CqlQuery query;

    private CqlSelectResult lastQueryResult;

    private final CsvQueryResultExporter exporter;

    private final static AppConfig.CqlExport conf = AppConfig.get().cqlExport;

    public QueryResultExport(MarkupContainer parent, CsvQueryResultExporter exporter) {
        this.exporter = exporter;
        this.downloader = new Downloader();
        parent.add(downloader);
    }

    public void initiateDownload(AjaxRequestTarget target, CqlQuery query, CqlSelectResult lastQueryResult) {
        downloader.initiateDownload(target);
        this.query = query;
        this.lastQueryResult = lastQueryResult;
    }

    private final class Downloader extends DownloadBehavior {

        @Override
        protected String getFileName() {
            SimpleDateFormat formatter = new SimpleDateFormat(conf.fileNameDate);
            return conf.fileName.replace("DATE", formatter.format(new Date()));
        }

        @Override
        protected IResourceStream getResourceStream() {
            if (query == null || lastQueryResult == null) {
                return new StringResourceStream("No Data");
            }

            String csv = exporter.createCsv(query, lastQueryResult);
            return new StringResourceStream(csv);

        }
    }


}
