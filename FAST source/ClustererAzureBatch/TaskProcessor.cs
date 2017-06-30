using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.Azure.Batch.Apps.Cloud;

namespace ClustererAzureBatch
{
    /// <summary>
    /// Processes a task.
    /// </summary>
    public class ClustererAzureBatchTaskProcessor : ParallelTaskProcessor
    {
        /// <summary>
        /// Executes the external process for processing the task
        /// </summary>
        /// <param name="task">The task to be processed.</param>
        /// <param name="settings">Contains information about the processing request.</param>
        /// <returns>The result of task processing.</returns>
        protected override TaskProcessResult RunExternalTaskProcess(ITask task, TaskExecutionSettings settings)
        {
            var process = new ExternalProcess
                {
                    CommandPath = ExecutablePath(@"TitanSpikeClusterer.exe"),
                    Arguments = string.Format("ClusterWorkerBlockRange {8} {0} {1} {2} {3} {4} {5} {6} {7}",
                        task.Parameters["HostName"],
                        task.Parameters["DataFileName"],
                        task.Parameters["NumChannels"],
                        task.Parameters["OutFileName"],
                        task.Parameters["MinTemp"],
                        task.Parameters["MaxTemp"],
                        task.Parameters["FirstBlock"],
                        task.Parameters["NumBlocks"],
                        LocalPath("Clusterer")),
                    WorkingDirectory = LocalStoragePath
                };

            try
            {
                ExternalProcessResult processOutput = process.Run();
                return new TaskProcessResult { Success = TaskProcessSuccess.Succeeded };
            }
            catch (ExternalProcessException ex) 
            { 
                string outputInfo = "No program output"; 
                if (!string.IsNullOrEmpty(ex.StandardError) || !string.IsNullOrEmpty(ex.StandardOutput)) 
                { 
                    outputInfo = Environment.NewLine + "stderr: " + ex.StandardError + Environment.NewLine + "stdout: " + ex.StandardOutput; 
                } 
 
                Log.Error("Failed to invoke command {0} {1}: exit code was {2}.  {3}", ex.CommandPath, ex.Arguments, ex.ExitCode, outputInfo); 
            } 
            catch (Exception ex) 
            { 
                Log.Error("Error in task processor: {0}", ex.ToString()); 
            } 
 
            return new TaskProcessResult { Success = TaskProcessSuccess.RetryableFailure };              
        }

        /// <summary>
        /// Method to execute the external processing for merging the tasks output into job output
        /// </summary>
        /// <param name="mergeTask">The merge task.</param>
        /// <param name="settings">Contains information about the processing request.</param>
        /// <returns>The job outputs resulting from the merge process.</returns>
        protected override JobResult RunExternalMergeProcess(ITask mergeTask, TaskExecutionSettings settings)
        {
            return new JobResult();
        }
    }
}
