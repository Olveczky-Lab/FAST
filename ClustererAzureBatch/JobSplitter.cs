using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Azure.Batch.Apps.Cloud;
using System.IO;

namespace ClustererAzureBatch
{
    /// <summary>
    /// Splits a job into tasks.
    /// </summary>
    public class ClustererAzureBatchJobSplitter : JobSplitter
    {
        /// <summary>
        /// Splits a job into more granular tasks to be processed in parallel.
        /// </summary>
        /// <param name="job">The job to be split.</param>
        /// <param name="settings">Contains information and services about the split request.</param>
        /// <returns>A sequence of tasks to be run on compute nodes.</returns>
        protected override IEnumerable<TaskSpecifier> Split(IJob job, JobSplitSettings settings)
        {
            try {
                var tasks = File.ReadAllLines(job.Files[0].Name)
                    .Select(line => {
                        var ps = line.Split('\t');
                        var task = new TaskSpecifier();
                        task.Parameters.Add("HostName",ps[0]);
                        task.Parameters.Add("DataFileName",ps[1]);
                        task.Parameters.Add("NumChannels",ps[2]);
                        task.Parameters.Add("OutFileName",ps[3]);
                        task.Parameters.Add("MinTemp",ps[4]);
                        task.Parameters.Add("MaxTemp",ps[5]);
                        task.Parameters.Add("FirstBlock",ps[6]);
                        task.Parameters.Add("NumBlocks",ps[7]);
                        return task;
                    });
                return tasks;
            }
            catch (Exception ex) {
                Log.Error("Error in job splitter: {0}", ex.ToString()); 
            }
            return Enumerable.Empty<TaskSpecifier>();
        }
    }
}
