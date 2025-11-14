# pminternal

<details>

* Version: 0.1.0
* GitHub: https://github.com/stephenrho/pminternal
* Source code: https://github.com/cran/pminternal
* Date/Publication: 2025-03-18 08:50:02 UTC
* Number of recursive dependencies: 122

Run `revdepcheck::revdep_details(, "pminternal")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pminternal-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: boot_optimism
    > ### Title: Calculate optimism and bias-corrected scores via bootstrap
    > ###   resampling
    > ### Aliases: boot_optimism
    > 
    > ### ** Examples
    > 
    ...
    > pred_fun <- function(model, data, ...){
    +   predict(model, newdata=data, type="response")
    + }
    > 
    > boot_optimism(data=dat, outcome="y", model_fun=model_fun, pred_fun=pred_fun,
    +               method="boot", B=20) # B set to 20 for example but should be >= 200
    Error in serverSocket(port = port) : 
      creation of server socket failed: port 11465 cannot be opened
    Calls: boot_optimism -> <Anonymous> -> makePSOCKcluster -> serverSocket
    Execution halted
    ```

