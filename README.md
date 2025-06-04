# Webhooks
Esse projeto visa montar uma estrutura de pagamentos usando uma arquitetura de webhooks. O client, nesse caso um servidor python, faz uma série de requisições para transações, e o servidor de webhooks confirma ou cancela as transações baseado em uma série de fatores. O cliente em python não foi escrito por mim, e sim pelo professor Raul Ikeda, como parte do curso de programação funcional, do Insper.


## Instalando e rodando

O programa foi testado usando o Python 3.12.8 e o Cabal 3.0.

Para baixar os requerimentos do servidor python, um requirements.txt está no diretório do servidor.

Para instalar o cabal, a recomendação é via o (GHCUP)[https://www.haskell.org/ghcup/].

Para fazer a instalação em sistemas unix, basta rodar o seguinte comando na sua shell.

```curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh```bash

Com o cabal instalado, faça o download desse repo, e dentro dele, rode o comando 
```cabal build``` para compilar o programa, ou ```cabal run``` para compilar e rodar ele.

Para rodar o servidor python, basta entrar do diretório python-server, rodar ```pip install -r requirements.txt```

Segue uma lista das features adicionais implementadas nesse servidor
- O serviço deve verificar a integridade do payload
- O serviço deve implementar algum mecanismo de veracidade da transação
- O serviço deve cancelar a transação em caso de divergência
- O serviço deve confirmar a transação em caso de sucesso
- O serviço deve persistir a transação em um BD
