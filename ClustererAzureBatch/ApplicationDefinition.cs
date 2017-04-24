using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Azure.Batch.Apps.Cloud;

namespace ClustererAzureBatch
{
    public class ApplicationDefinition
    {
        public static readonly CloudApplication Application = new ParallelCloudApplication
            {
                ApplicationName = "ClustererAzureBatch",
                JobType = "TitanSpikeClusterer",
                JobSplitterType = typeof(ClustererAzureBatchJobSplitter),
                TaskProcessorType = typeof(ClustererAzureBatchTaskProcessor)
            };

    }
}
