(defun $dashboard-isc-threat-level (list-size)
  (request "https://isc.sans.edu/infocon.txt"
           :success
           (cl-function
            (lambda (&key data &allow-other-keys)
              (insert
               (concat
                "ISC Status: "
                (capitalize data)))))))

