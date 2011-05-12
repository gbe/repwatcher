type certs_t = {
  c_ca_path : string;
  c_client_cert_path : string;
  c_client_key_path : string;
  c_client_key_pwd : string option;
}

type configuration = {
    c_certs : certs_t
}
